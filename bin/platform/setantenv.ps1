$env:ANT_OPTS="-Xmx2g -Dfile.encoding=UTF-8"
$env:ANT_HOME="$pwd\apache-ant"
$env:Path="$env:ANT_HOME\bin;$env:Path"
echo "Setting ant home to: $env:ANT_HOME"
ant -version
