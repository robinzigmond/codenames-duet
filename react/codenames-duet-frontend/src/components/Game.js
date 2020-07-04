import React, { useState } from 'react';
import useMessageInput from '../hooks/useMessageInput';

import Card from './Card';
import ClueInput from './ClueInput';

const Game = (props) => {
  const [isClueing, setisClueing] = useState(true);
  const [clueGiven, setClueGiven] = useState(null);

  const sendMessage = useMessageInput((received) => {
    if (received) {
      console.log(received);
      const { type, message } = received;
      switch (type) {
        case 'ClueReceived':
          setisClueing(false);
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

  const onClueGiven = () => { setisClueing(false); }

  const guessCard = (row, col) => () => {
    if (clueGiven) {
      console.log("SENDING GUESS:");
      console.log([row, col]);
      sendMessage({ type: 'CardGuessed', message: [row, col] });
    }
  }

  return (
    <React.Fragment>
      {clueGiven ? (
        <div>
          <p>Clue given by your partner:</p>
          <p>{`${clueGiven[0]} - ${clueGiven[1]}`}</p>
        </div>
      ) : isClueing
          ? null
          : <p>Your partner is guessing!</p>
      }
      {props.cardState.map((row, rowNo) => (
        <div>
          {row.map(({ word, type }, colNo) => (
            <Card
              cardText={word}
              type={type.toLowerCase()}
              guessMode={!!clueGiven}
              guessCard={guessCard(rowNo + 1, colNo + 1)}
            />
          ))}
        </div>
      ))}
      {isClueing && <ClueInput allWords={allWords} onClueGiven={onClueGiven} />}
    </React.Fragment>
  );
};

export default Game;
