import React, { useEffect, useState } from 'react';
import useWebSocket from 'react-use-websocket';

const App = () => {
  const [cards, setCards] = useState([]);

  const {
    //sendMessage,
    lastMessage//,
    //readyState,
  } = useWebSocket('ws://localhost:3000');

  useEffect(() => {
    if (lastMessage) {
      console.log(`lastMessage data is ${lastMessage.data}`);
      setCards(JSON.parse(lastMessage.data));
    }
  }, [lastMessage])

  return (
    <React.Fragment>
      <h1>Codenames duet game!</h1>
      <div>
        {cards.map(row => (
          <div>
            {row.map(card => (
              <div style={{ display: 'inline-block', margin: '10px', width: '50px', height: '20px' }}>{card}</div>
            ))}
          </div>
        ))}
      </div>
    </React.Fragment>
  )
};

export default App;
