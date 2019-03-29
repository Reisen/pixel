# Manage Repo Configuration in Git Itself
# ------------------------------------------------------------------------------
# The main reason for this is it makes it easy to duplicate issue labels
# and certain settings accross related repositories.

provider "github" {
    organization = "Reisen"
}

variable "category_color" { default = "335577" }
variable "danger_color"   { default = "CC4432" }
variable "info_color"     { default = "f9ce70" }
variable "help_color"     { default = "008672" }
variable "no_color"       { default = "FFFFFF" }

# ------------------------------------------------------------------------------

resource "github_repository" "pixel" {
    name          = "pixel"
    description   = "A private self-hosted gallery and image feed."
    homepage_url  = "https://pixelized.netlify.com"
    private       = false
    has_downloads = true
    has_issues    = true
    has_wiki      = true
    homepage_url  = "https://pixelized.netlify.com"
    topics        = [
        "self-hosted",
        "gallery"
    ]
}

# ------------------------------------------------------------------------------

# resource "github_issue_label" "alteration" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "👌🏻 alteration"
#   color       = "${var.category_color}"
#   description = "Small changes and tweaks."
# }
#
# resource "github_issue_label" "backend" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "⚙️ backend"
#   color       = "${var.category_color}"
#   description = "This requires backend fixes."
# }
#
# resource "github_issue_label" "bug" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "👻 bug "
#   color       = "${var.danger_color}"
#   description = "Something isn't working"
# }
#
# resource "github_issue_label" "duplicate" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "⁉️ duplicate"
#   color       = "${var.info_color}"
#   description = "This issue or pull request already exists"
# }
#
# resource "github_issue_label" "enhancement" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "🥒 enhancement"
#   color       = "${var.category_color}"
#   description = "New feature or request"
# }
#
# resource "github_issue_label" "help" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "🥼 help wanted"
#   color       = "${var.help_color}"
#   description = "Call backup."
# }
#
# resource "github_issue_label" "invalid" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "⁉️ invalid"
#   color       = "${var.info_color}"
#   description = "This doesn't seem right"
# }
#
# resource "github_issue_label" "question" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "⁉️ question"
#   color       = "${var.info_color}"
#   description = "Further information is requested"
# }
#
# resource "github_issue_label" "security" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "⚔️ security"
#   color       = "${var.danger_color}"
#   description = "This issue has security implications."
# }
#
# resource "github_issue_label" "tracking" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "🖇 tracking"
#   color       = "${var.no_color}"
#   description = "Tracking issues."
# }
#
# resource "github_issue_label" "web" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "🌎 web"
#   color       = "${var.category_color}"
#   description = "This requires frontend fixes."
# }
#
# resource "github_issue_label" "wontfix" {
#   repository  = "${github_repository.pixel.name}"
#   name        = "❌ wontfix"
#   color       = "${var.no_color}"
#   description = "This will not be worked on"
# }
