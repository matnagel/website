variable "project" {
  type = string
}

variable "region" {
  type = string
}

variable "deployment_bucket" {
  type = string
}

variable "versions" {
  type = set(string)
}
