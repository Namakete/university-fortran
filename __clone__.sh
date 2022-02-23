#!/bin/bash

read -p "Enter fullname: " folder # Enter the name of the folder

mkdir $folder # Create a folder

cp -rv .maket/ $folder/ # Copies and pastes configuration files from '.layout/' to 'the created folder'


