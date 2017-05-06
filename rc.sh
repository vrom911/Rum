cd ../..
gradle run -q -Dmyvariable=myvalue -Dexec.args="$1 compiler-tests/core/$2" --profile --daemon