java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=512m -Xmx1512M -Xms512M -jar `dirname $0`/sbt-launcher-0.12.1.jar "$@"
