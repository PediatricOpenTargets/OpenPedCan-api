@Library(value='kids-first/aws-infra-jenkins-shared-libraries', changelog=false) _
ecs_service_type_1_standard {
    projectName = "openpedcan-api"
    internal_app = "false"
    docker_image_type = "debian"
    entrypoint_command = "Rscript --vanilla main.R"
    quick_deploy = "true"
    container_port = "80"
    health_check_path = "/__docs__/"
    dependencies = "ecr"
    prd_cidr = "0.0.0.0/0"
    qa_cidr = "0.0.0.0/0"
    dev_cidr = "0.0.0.0/0"
    desired_count = "5"
    vcpu_container             = "4096"
    memory_container           = "10240"
    vcpu_task                  = "4096"
    memory_task                = "10240"
}
