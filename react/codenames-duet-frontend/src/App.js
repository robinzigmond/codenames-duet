import React, { useEffect, useState } from 'react';
import useWebSocket from 'react-use-websocket';
import { useHistory, useLocation } from 'react-router-dom';

const App = () => {
  const [cards, setCards] = useState([]);
  const [error, setError] = useState('');
  const location = useLocation();
  const history = useHistory();
  const gameId = location.pathname.slice(1);

  const {
    sendJsonMessage,
    lastJsonMessage
  } = useWebSocket('ws://localhost:3000');

  useEffect(() => {
    if (lastJsonMessage) {
      console.log(lastJsonMessage);
      const { type, message } = lastJsonMessage;
      switch (type) {
        case 'CardsForGame':
          setCards(message);
          break;
        case 'GameStarted':
          const [id, newCards] = message;
          setCards(newCards);
          history.push(`/${id}`);
          break;
        case 'CantJoin':
          setError(message);
          break;
        default:
          break;
      }
    }
  }, [lastJsonMessage]);

  useEffect(() => {
    if (gameId) {
      sendJsonMessage({ type: 'JoinedGame', message: gameId });
    }
  }, [gameId]);

  const onNewGame = () => {
    sendJsonMessage({ type: 'NewGame' });
  };

  return (
    <React.Fragment>
      <h1>Codenames duet game!</h1>
      <div>
        <p>{error}</p>
        {gameId ?
          <React.Fragment>
            <p>Game id {gameId}</p>
            {cards.map(row => (
              <div>
                {row.map(card => (
                  <div style={{ display: 'inline-block', margin: '10px', width: '150px', height: '50px' }}>{card}</div>
                ))}
              </div>
            ))}
          </React.Fragment>
          : <button onClick={onNewGame}>
            New Game
            </button>}
      </div>
    </React.Fragment>
  );
};

export default App;
