variable "gitlab_runner_token" {
  description = "The registration token provided by gitlab when creating a project runner"
  type        = string
  sensitive   = true
}
