import React, { useState, useEffect } from 'react';
import useMessageInput from '../hooks/useMessageInput';

import Card from './Card';
import ClueInput from './ClueInput';

const Game = (props) => {
  const [inputShown, setInputShown] = useState(true);

  const sendMessage = useMessageInput((received) => {
    if (received) {
      console.log(received);
      const { type, message } = received;
      switch (type) {

        default:
          break;
      }
    }
  });

  const getWord = cardObj => cardObj.word;

  const allWords = props.cardState
    .reduce((all, row) => [...all, ...row.map(getWord)], []);

  return (
    <React.Fragment>
      {props.cardState.map(row => (
        <div>
          {row.map(({ word, type }) => (
            <Card cardText={word} type={type.toLowerCase()} />
          ))}
        </div>
      ))}
      {props.playerNum && (<p>I am player {props.playerNum}!</p>)}
      {inputShown && <ClueInput allWords={allWords} />}
    </React.Fragment>
  );
};

export default Game;
