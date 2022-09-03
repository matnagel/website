variable "project" {
  type = string
}

variable "region" {
  type = string
}

variable "deployment_bucket" {
  type = string
}

variable "all_versions" {
  type = set(string)
}

variable "current_version" {
  type = string
}
