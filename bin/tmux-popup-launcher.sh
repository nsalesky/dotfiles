#!/usr/bin/env bash

options=(
  "lazygit"
  "yazi"
)

choice=$(printf "%s\n" "${options[@]}" | fzf)
  
$choice
