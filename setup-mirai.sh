#!/bin/bash
mkdir mirai
cd mirai
mkdir libs
cd libs
curl -L https://maven.aliyun.com/repository/public/net/mamoe/mirai-core-all/1.3.3/mirai-core-all-1.3.3-all.jar -o mirai-core-all-1.3.3.jar
curl -L https://maven.aliyun.com/repository/public/net/mamoe/mirai-console/1.1.0/mirai-console-1.1.0-all.jar -o mirai-console-1.1.0.jar
curl -L https://maven.aliyun.com/repository/public/net/mamoe/mirai-console-terminal/1.1.0/mirai-console-terminal-1.1.0-all.jar -o mirai-console-terminal-1.1.0.jar
cd ..
mkdir plugins
cd plugins
wget https://github.com/project-mirai/mirai-api-http/releases/download/v1.9.0/mirai-api-http-v1.9.0.jar/
cd ..
cat << EOF > start.sh
#!/bin/bash
java -cp "./libs/*" net.mamoe.mirai.console.terminal.MiraiConsoleTerminalLoader $*
EOF
chmod +x start.sh