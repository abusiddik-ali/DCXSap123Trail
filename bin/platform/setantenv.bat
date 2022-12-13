@echo off
set ANT_OPTS=-Xmx2g -Dfile.encoding=UTF-8
set ANT_HOME=%~dp0apache-ant
set PATH=%ANT_HOME%\bin;%PATH%
rem deleting CLASSPATH as a workaround for PLA-8702
set CLASSPATH=

echo ant home: %ANT_HOME%
echo ant opts: %ANT_OPTS%
ant -version
