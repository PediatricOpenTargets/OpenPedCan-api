@Library(value='kids-first/aws-infra-jenkins-shared-libraries', changelog=false) _
ecs_service_type_1_standard {
    projectName = "openpedcan-api"
    internal_app = "false"
    environments = "dev,qa,prd"
    entrypoint_command = "Rscript main.R"
    quick_deploy = "true"
    container_port = "80"
    health_check_path = "/"
    dependencies = "ecr"
}