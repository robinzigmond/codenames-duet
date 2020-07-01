import React, { useState } from 'react';
import useMessageInput from '../hooks/useMessageInput';

import Card from './Card';
import ClueInput from './ClueInput';

const Game = (props) => {
  const [inputShown, setInputShown] = useState(true);
  const [clueGiven, setClueGiven] = useState(null);

  useMessageInput((received) => {
    if (received) {
      console.log(received);
      const { type, message } = received;
      switch (type) {
        case 'ClueReceived':
          setInputShown(false);
          setClueGiven(message);
          break;
        default:
          break;
      }
    }
  });

  const getWord = cardObj => cardObj.word;

  const allWords = props.cardState
    .reduce((all, row) => [...all, ...row.map(getWord)], []);

  const onClueGiven = () => { setInputShown(false); }

  return (
    <React.Fragment>
      {clueGiven ? (
        <div>
          <p>Clue given by your partner:</p>
          <p>{`${clueGiven[0]} - ${clueGiven[1]}`}</p>
        </div>
      ) : inputShown
          ? null
          : <p>Waiting for your partner...</p>
      }
      {props.cardState.map(row => (
        <div>
          {row.map(({ word, type }) => (
            <Card cardText={word} type={type.toLowerCase()} />
          ))}
        </div>
      ))}
      {inputShown && <ClueInput allWords={allWords} onClueGiven={onClueGiven} />}
    </React.Fragment>
  );
};

export default Game;
