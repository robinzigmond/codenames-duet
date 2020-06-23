import React, { useEffect, useState } from 'react';
import useWebSocket from 'react-use-websocket';
import { useHistory, useLocation } from 'react-router-dom';
import styled from 'styled-components';

import Game from './components/Game';

const StyledApp = styled.div`
  & {
    max-width: 1000px;
    margin: 50px auto;
    display: flex;
    flex-direction: column;
    align-items: center;

    h1 {
      text-align: center;
    }

    button {
      width: 180px;
      height: 65px;
      background-color: #ccb43b;
      font-size: 18px;
      font-weight: bold;
      color: #2a2952;
      cursor: pointer;
    }
  }
`;

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
    <StyledApp>
      <h1>Codenames Duet</h1>
      <div>
        <p>{error}</p>
        {gameId ?
          <Game cards={cards} />
          : <button onClick={onNewGame}>
            NEW GAME
            </button>}
      </div>
    </StyledApp>
  );
};

export default App;
