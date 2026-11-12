#!/usr/bin/env bash
# One-time provisioning of the testnet node on Azure (safe to re-run: existing resources are kept).
#
#   deploy/azure/setup.sh
#
# Creates: resource group, VM with a data disk and static IP, firewall rules, Log Analytics workspace with the
# node's log table, Azure Monitor agent and collection rule, alert rules, and a GitHub OIDC identity that may only
# run commands on this VM. Needs: az (logged in), gh (logged in), ssh-keygen, curl, jq.
set -euo pipefail
cd "$(dirname "$0")"

RG="${RG:-jam-testnet-rg}"
LOC="${LOC:-spaincentral}"           # nearest region to the bootnode (Portugal)
VM="${VM:-jam-testnet-node}"
SIZE="${SIZE:-Standard_D4as_v5}"
ADMIN="${ADMIN:-jam}"
DATA_DISK_GB="${DATA_DISK_GB:-128}"
SSH_KEY="${SSH_KEY:-$HOME/.ssh/jam-testnet_ed25519}"
SSH_FROM="${SSH_FROM:-$(curl -fsS https://api.ipify.org)/32}"
SPEC_URL="${SPEC_URL:-https://jamtoaster.network/chainspec/public.json}"
JAM_ARGS="${JAM_ARGS:---finality-peers 1 --prune-history}"
WS="${WS:-jam-testnet-logs}"
REPO="${REPO:-philoniare/jam-forge}"
APP="${APP:-jam-forge-testnet-deploy}"

SUB=$(az account show --query id -o tsv)
TENANT=$(az account show --query tenantId -o tsv)
say() { echo "== $*"; }

say "resource group $RG ($LOC)"
az group create -n "$RG" -l "$LOC" -o none

# ---------------------------------------------------------------------------------------------------------------
# VM
# ---------------------------------------------------------------------------------------------------------------
[[ -f "$SSH_KEY" ]] || ssh-keygen -q -t ed25519 -N "" -C "$VM" -f "$SSH_KEY"

if ! az vm show -g "$RG" -n "$VM" -o none 2>/dev/null; then
  say "VM $VM ($SIZE)"
  b64() { base64 < "$1" | tr -d '\n'; }
  CLOUD_INIT=$(mktemp)
  sed -e "s|@@SPEC_URL@@|$SPEC_URL|" -e "s|@@JAM_ARGS@@|$JAM_ARGS|" -e "s|@@ADMIN_USER@@|$ADMIN|" \
      -e "s|@@LOGBACK_B64@@|$(b64 logback.xml)|" -e "s|@@UNIT_B64@@|$(b64 jam-node.service)|" \
      -e "s|@@DEPLOY_B64@@|$(b64 deploy.sh)|" cloud-init.yaml.tpl > "$CLOUD_INIT"
  az vm create -g "$RG" -n "$VM" -l "$LOC" --size "$SIZE" \
    --image Canonical:ubuntu-24_04-lts:server:latest \
    --admin-username "$ADMIN" --ssh-key-values "$SSH_KEY.pub" \
    --os-disk-size-gb 64 --storage-sku Premium_LRS \
    --data-disk-sizes-gb "$DATA_DISK_GB" --data-disk-caching ReadOnly \
    --public-ip-sku Standard --public-ip-address-allocation static --public-ip-address "$VM-ip" \
    --nsg "$VM-nsg" --nsg-rule NONE \
    --assign-identity --custom-data "$CLOUD_INIT" -o none
  rm -f "$CLOUD_INIT"
fi
VM_ID=$(az vm show -g "$RG" -n "$VM" --query id -o tsv)
IP=$(az network public-ip show -g "$RG" -n "$VM-ip" --query ipAddress -o tsv)

say "firewall: JAMNP-S UDP 40000 open, SSH from $SSH_FROM only"
az network nsg rule create -g "$RG" --nsg-name "$VM-nsg" -n allow-jamnp --priority 100 \
  --protocol Udp --destination-port-ranges 40000 --access Allow --direction Inbound -o none
az network nsg rule create -g "$RG" --nsg-name "$VM-nsg" -n allow-ssh --priority 110 \
  --protocol Tcp --destination-port-ranges 22 --source-address-prefixes "$SSH_FROM" \
  --access Allow --direction Inbound -o none

# ---------------------------------------------------------------------------------------------------------------
# Logs: workspace, node log table, agent, collection rule
# ---------------------------------------------------------------------------------------------------------------
say "Log Analytics workspace $WS"
az monitor log-analytics workspace create -g "$RG" -n "$WS" -l "$LOC" --retention-time 30 -o none
WS_ID=$(az monitor log-analytics workspace show -g "$RG" -n "$WS" --query id -o tsv)
az monitor log-analytics workspace table create -g "$RG" --workspace-name "$WS" -n JamNodeLog_CL \
  --columns TimeGenerated=datetime RawData=string FilePath=string Computer=string -o none 2>/dev/null || true

DCE_ID="/subscriptions/$SUB/resourceGroups/$RG/providers/Microsoft.Insights/dataCollectionEndpoints/$VM-dce"
az rest -m put -u "https://management.azure.com$DCE_ID?api-version=2022-06-01" \
  -b "{\"location\":\"$LOC\",\"properties\":{\"networkAcls\":{\"publicNetworkAccess\":\"Enabled\"}}}" -o none

DCR_ID="/subscriptions/$SUB/resourceGroups/$RG/providers/Microsoft.Insights/dataCollectionRules/$VM-dcr"
jq -n --arg loc "$LOC" --arg dce "$DCE_ID" --arg ws "$WS_ID" '{
  location: $loc,
  kind: "Linux",
  properties: {
    dataCollectionEndpointId: $dce,
    streamDeclarations: { "Custom-JamNodeLog_CL": { columns: [
      {name: "TimeGenerated", type: "datetime"}, {name: "RawData", type: "string"},
      {name: "FilePath", type: "string"}, {name: "Computer", type: "string"} ] } },
    dataSources: {
      logFiles: [{ name: "jamNodeLog", streams: ["Custom-JamNodeLog_CL"], format: "text",
                   filePatterns: ["/var/log/jam-node/node.log"],
                   settings: { text: { recordStartTimestampFormat: "ISO 8601" } } }],
      performanceCounters: [{ name: "perf", streams: ["Microsoft-Perf"], samplingFrequencyInSeconds: 60,
        counterSpecifiers: ["Processor(*)\\% Processor Time", "Memory(*)\\% Used Memory",
                            "Logical Disk(*)\\% Used Space", "Network(*)\\Total Bytes"] }],
      syslog: [{ name: "syslog", streams: ["Microsoft-Syslog"], facilityNames: ["daemon", "kern", "syslog"],
                 logLevels: ["Warning", "Error", "Critical", "Alert", "Emergency"] }]
    },
    destinations: { logAnalytics: [{ name: "ws", workspaceResourceId: $ws }] },
    dataFlows: [
      { streams: ["Custom-JamNodeLog_CL"], destinations: ["ws"], transformKql: "source",
        outputStream: "Custom-JamNodeLog_CL" },
      { streams: ["Microsoft-Perf", "Microsoft-Syslog"], destinations: ["ws"] }
    ]
  } }' > /tmp/jam-dcr.json
say "data collection rule"
az rest -m put -u "https://management.azure.com$DCR_ID?api-version=2022-06-01" -b @/tmp/jam-dcr.json -o none
rm -f /tmp/jam-dcr.json
az rest -m put -o none \
  -u "https://management.azure.com$VM_ID/providers/Microsoft.Insights/dataCollectionRuleAssociations/$VM-dcr?api-version=2022-06-01" \
  -b "{\"properties\":{\"dataCollectionRuleId\":\"$DCR_ID\"}}"
az rest -m put -o none \
  -u "https://management.azure.com$VM_ID/providers/Microsoft.Insights/dataCollectionRuleAssociations/configurationAccessEndpoint?api-version=2022-06-01" \
  -b "{\"properties\":{\"dataCollectionEndpointId\":\"$DCE_ID\"}}"

say "Azure Monitor agent"
az vm extension set -g "$RG" --vm-name "$VM" -n AzureMonitorLinuxAgent \
  --publisher Microsoft.Azure.Monitor --enable-auto-upgrade true -o none

# ---------------------------------------------------------------------------------------------------------------
# Alerts (the action group starts without receivers: add yours with the command printed at the end)
# ---------------------------------------------------------------------------------------------------------------
say "alert rules"
az monitor action-group create -g "$RG" -n jam-testnet-alerts --short-name jamalerts -o none
AG_ID=$(az monitor action-group show -g "$RG" -n jam-testnet-alerts --query id -o tsv)

alert() { # name severity window(min) operator threshold query
  local id="/subscriptions/$SUB/resourceGroups/$RG/providers/Microsoft.Insights/scheduledQueryRules/$1"
  jq -n --arg loc "$LOC" --arg ws "$WS_ID" --arg ag "$AG_ID" --arg name "$1" --argjson sev "$2" \
    --arg win "PT$3M" --arg op "$4" --argjson th "$5" --arg q "$6" '{
    location: $loc,
    properties: { displayName: $name, severity: $sev, enabled: true, scopes: [$ws],
      evaluationFrequency: "PT5M", windowSize: $win, autoMitigate: true,
      criteria: { allOf: [{ query: $q, timeAggregation: "Count", operator: $op, threshold: $th,
        failingPeriods: { numberOfEvaluationPeriods: 1, minFailingPeriodsToAlert: 1 } }] },
      actions: { actionGroups: [$ag] } } }' > /tmp/jam-alert.json
  az rest -m put -u "https://management.azure.com$id?api-version=2021-08-01" -b @/tmp/jam-alert.json -o none
  rm -f /tmp/jam-alert.json
}
alert jam-node-errors 2 5 GreaterThan 0 'JamNodeLog_CL | where RawData has " ERROR "'
alert jam-node-import-stalled 1 10 LessThan 1 'JamNodeLog_CL | where RawData has "imported block"'
alert jam-node-finality-stalled 2 10 GreaterThan 0 'JamNodeLog_CL | where RawData has "finalized=" | extend f = tolong(extract("finalized=([0-9]+)", 1, RawData)) | summarize d = max(f) - min(f), n = count() | where n > 5 and d == 0'
alert jam-node-restarted 3 5 GreaterThan 0 'JamNodeLog_CL | where RawData has "node up:"'
alert jam-node-disk-80pct 2 15 GreaterThan 0 'Perf | where ObjectName == "Logical Disk" and CounterName == "% Used Space" and InstanceName == "/var/lib/jam" | summarize v = avg(CounterValue) | where v > 80'
alert jam-node-heartbeat-missing 1 10 LessThan 1 "Heartbeat | where Computer == \"$VM\""

# ---------------------------------------------------------------------------------------------------------------
# GitHub Actions: OIDC identity allowed to run commands on this VM only, and the repo secrets that name it
# ---------------------------------------------------------------------------------------------------------------
say "GitHub OIDC identity $APP"
APP_ID=$(az ad app list --display-name "$APP" --query "[0].appId" -o tsv)
[[ -n "$APP_ID" ]] || APP_ID=$(az ad app create --display-name "$APP" --query appId -o tsv)
az ad sp show --id "$APP_ID" -o none 2>/dev/null || az ad sp create --id "$APP_ID" -o none
if ! az ad app federated-credential list --id "$APP_ID" --query "[?name=='main']" -o tsv | grep -q .; then
  az ad app federated-credential create --id "$APP_ID" -o none --parameters "{
    \"name\": \"main\", \"issuer\": \"https://token.actions.githubusercontent.com\",
    \"subject\": \"repo:$REPO:ref:refs/heads/main\", \"audiences\": [\"api://AzureADTokenExchange\"]}"
fi
SP_ID=$(az ad sp show --id "$APP_ID" --query id -o tsv)
az role assignment create --assignee-object-id "$SP_ID" --assignee-principal-type ServicePrincipal \
  --role "Virtual Machine Contributor" --scope "$VM_ID" -o none

say "repo secrets on $REPO"
gh secret set AZURE_CLIENT_ID -R "$REPO" -b "$APP_ID"
gh secret set AZURE_TENANT_ID -R "$REPO" -b "$TENANT"
gh secret set AZURE_SUBSCRIPTION_ID -R "$REPO" -b "$SUB"
gh variable set AZURE_RG -R "$REPO" -b "$RG"
gh variable set AZURE_VM -R "$REPO" -b "$VM"

cat <<EOF

Done. VM $VM at $IP (JAMNP-S UDP 40000), ssh -i $SSH_KEY $ADMIN@$IP
The node starts on the first deploy (push to main, or run the "Deploy testnet node" workflow).
Alerts go nowhere until you add a receiver:
  az monitor action-group update -g $RG -n jam-testnet-alerts --add-action email me <your-email>
EOF
