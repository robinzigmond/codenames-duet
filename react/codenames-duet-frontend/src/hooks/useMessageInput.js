import { useEffect } from 'react';
import useWebSocket from 'react-use-websocket';

const useMessageInput = (responseFunction) => {
  const {
    sendJsonMessage,
    lastJsonMessage
  } = useWebSocket('ws://localhost:3000');

  useEffect(() => {
    responseFunction(lastJsonMessage);
  }, [lastJsonMessage]);

  return sendJsonMessage;
}

export default useMessageInput;