RED='\033[0;31m'
LIMIT=240
minutes=0

echo -e "RUNNING: $@ in the background"

# send the long living command to background
eval $@ & jobs

while kill -0 $! >/dev/null 2>&1; do
  # Do some untracable work
  echo -n -e " \b"

  if [ $minutes == $LIMIT ]; then
    echo -e "\n"
    echo -e "${RED}Test has reached a ${minutes} minutes timeout limit"
    kill -- -$$
    exit 1
  fi

  minutes=$((minutes+1))

  sleep 60
done

exit 0
