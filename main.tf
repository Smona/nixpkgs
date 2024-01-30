terraform {
  cloud {
    organization = "Smona"

    workspaces {
      name = "nixpkgs"
    }
  }
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.16"
    }
  }

  required_version = ">= 1.2.0"
}

provider "aws" {
  region = "us-west-2"
}

module "nixos_image" {
  source  = "git::https://github.com/tweag/terraform-nixos.git//aws_image_nixos?ref=646cacb12439ca477c05315a7bfd49e9832bc4e3"
  release = "20.09"
}


resource "aws_security_group" "ssh_and_egress" {
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "tls_private_key" "state_ssh_key" {
  algorithm = "RSA"
}

resource "local_sensitive_file" "machine_ssh_key" {
  content         = tls_private_key.state_ssh_key.private_key_pem
  filename        = "${path.module}/id_rsa.pem"
  file_permission = "0600"
}

resource "aws_key_pair" "generated_key" {
  key_name   = "generated-key-${sha256(tls_private_key.state_ssh_key.public_key_openssh)}"
  public_key = tls_private_key.state_ssh_key.public_key_openssh
}

resource "aws_instance" "machine" {
  ami             = module.nixos_image.ami
  instance_type   = "t3.micro"
  security_groups = [aws_security_group.ssh_and_egress.name]
  key_name        = aws_key_pair.generated_key.key_name

  root_block_device {
    volume_size = 50 # GiB
  }
}

module "deploy_nixos" {
  source               = "git::https://github.com/tweag/terraform-nixos.git//deploy_nixos?ref=646cacb12439ca477c05315a7bfd49e9832bc4e3"
  nixos_config         = "build-farm"
  target_host          = aws_instance.machine.public_ip
  ssh_private_key_file = local_sensitive_file.machine_ssh_key.filename
  ssh_agent            = false
  flake                = true
}

output "public_dns" {
  value = aws_instance.machine.public_dns
}
