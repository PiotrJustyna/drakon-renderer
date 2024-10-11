$certificate_directory="$env:USERPROFILE/.ssh/"

Invoke-Expression -Command "docker buildx build -t drakon-renderer:latest -f dockerfile ."

Invoke-Expression -Command "docker run -it -v ${PWD}:/root/code/drakon-renderer -v ${certificate_directory}:/root/.ssh:ro --rm drakon-renderer:latest"
