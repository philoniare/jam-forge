#cloud-config
# VM bootstrap for the testnet node. setup.sh fills in the @@...@@ placeholders from the files next to it.
package_update: true
packages:
  - docker.io
  - curl
  - jq

write_files:
  - path: /etc/docker/daemon.json
    content: |
      {"log-driver": "json-file", "log-opts": {"max-size": "50m", "max-file": "5"}}
  - path: /etc/jam-node/env
    permissions: "0644"
    content: |
      JAM_IMAGE=
      SPEC_URL=@@SPEC_URL@@
      JAM_ARGS=@@JAM_ARGS@@
  - path: /etc/jam-node/logback.xml
    encoding: b64
    content: @@LOGBACK_B64@@
  - path: /etc/systemd/system/jam-node.service
    encoding: b64
    content: @@UNIT_B64@@
  - path: /opt/jam-node/deploy.sh
    permissions: "0755"
    encoding: b64
    content: @@DEPLOY_B64@@
  - path: /opt/jam-node/mount-data.sh
    permissions: "0755"
    content: |
      #!/bin/sh
      # Format the data disk (LUN 0) on first boot and mount it at /var/lib/jam.
      set -eu
      DEV=""
      for d in /dev/disk/azure/data/by-lun/0 /dev/disk/azure/scsi1/lun0; do [ -e "$d" ] && DEV=$(readlink -f "$d") && break; done
      [ -n "$DEV" ] || { echo "data disk not found" >&2; exit 1; }
      blkid "$DEV" >/dev/null 2>&1 || mkfs.ext4 -q -L jamdata "$DEV"
      mkdir -p /var/lib/jam
      grep -q 'LABEL=jamdata' /etc/fstab || echo 'LABEL=jamdata /var/lib/jam ext4 defaults,nofail,noatime 0 2' >> /etc/fstab
      mount -a

runcmd:
  - /opt/jam-node/mount-data.sh
  - mkdir -p /var/log/jam-node
  - systemctl restart docker
  - systemctl daemon-reload
  - usermod -aG docker @@ADMIN_USER@@
