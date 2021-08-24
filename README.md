# OpenPedCan-api

[![GitHub Super-Linter](https://github.com/PediatricOpenTargets/OpenPedCan-api/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter)

- [OpenPedCan-api](#openpedcan-api)
  - [Deploy `OpenPedCan-api`](#deploy-openpedcan-api)

## Deploy `OpenPedCan-api`

Following is a comment by @blackdenc at <https://github.com/PediatricOpenTargets/OpenPedCan-api/issues/5#issuecomment-904824004>.

> As far as building in Jenkins, we build the container and tag it, then push it to ECR and give that tag to the ECS task definition at runtime. Adding a workdir shouldn't affect deployment as long as the container still builds and runs locally.

`Rscript --vanilla main.R` needs to be run with the same working directory as the last `WORKDIR` path in `Dockerfile` prior to the docker instruction `ENTRYPOINT ["Rscript", "--vanilla", "main.R"]`.
