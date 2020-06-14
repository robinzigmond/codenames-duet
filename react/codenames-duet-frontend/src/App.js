import React, { useEffect, useState } from 'react';
import useWebSocket from 'react-use-websocket';
import { useLocation } from 'react-router-dom';

const App = () => {
  const [cards, setCards] = useState([]);
  const [gameId, setGameId] = useState(null);
  const location = useLocation();

  const {
    sendJsonMessage,
    lastJsonMessage
  } = useWebSocket('ws://localhost:3000');

  useEffect(() => {
    if (lastJsonMessage) {
      console.log(lastJsonMessage);
      setCards(lastJsonMessage);
    }
  }, [lastJsonMessage])

  useEffect(() => {
    if (location) {
      setGameId(location.pathname.slice(1));
    }
  }, [location])

  return (
    <React.Fragment>
      <h1>Codenames duet game!</h1>
      <div>
        {gameId ?
          <React.Fragment>
            <p>Game id {gameId}</p>
            {cards.map(row => (
              <div>
                {row.map(card => (
                  <div style={{ display: 'inline-block', margin: '10px', width: '50px', height: '20px' }}>{card}</div>
                ))}
              </div>
            ))}
          </React.Fragment>
          : 'No game started!'}
      </div>
    </React.Fragment>
  )
};

export default App;
