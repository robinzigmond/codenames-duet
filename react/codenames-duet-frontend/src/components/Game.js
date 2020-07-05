import React, { useState } from 'react';
import styled from 'styled-components';

import useMessageInput from '../hooks/useMessageInput';
import Card from './Card';
import ClueInput from './ClueInput';

const StopGuessingButton = styled.div`
  & {
    display: flex;
    justify-content: center;
  }

  & button.stop-guessing {
    width: auto;
    height: 40px;
    padding: 5px 20px;
    margin-top: 30px;
  }
`;

const Game = (props) => {
  const [isClueing, setIsClueing] = useState(true);
  const [isGuessing, setIsGuessing] = useState(false);
  const [clueGiven, setClueGiven] = useState(null);
  const [hasGuessed, setHasGuessed] = useState(false);
  const [isFirstTurn, setIsFirstTurn] = useState(true);
  const [partnerClueing, setPartnerClueing] = useState(false);

  const sendMessage = useMessageInput((received) => {
    if (received) {
      console.log(received);
      const { type, message } = received;
      switch (type) {
        case 'ClueReceived':
          setIsClueing(false);
          setIsGuessing(true);
          setHasGuessed(false);
          setClueGiven(message);
          setIsFirstTurn(false);
          break;
        case 'CardGuessedResponse':
          if (!isClueing) {
            setHasGuessed(true);
          }
          break;
        case 'GuessingStoppedResponse':
          setIsClueing(false);
          setIsGuessing(false);
          setClueGiven(null);
          setPartnerClueing(true);
          break;
        default:
          break;
      }
    }
  });

  const getWord = cardObj => cardObj.word;

  const allWords = props.cardState
    .reduce((all, row) => [...all, ...row.map(getWord)], []);

  const onClueGiven = () => {
    setIsFirstTurn(false);
    setIsClueing(false);
  }

  const guessCard = (row, col) => () => {
    if (clueGiven) {
      sendMessage({ type: 'CardGuessed', message: [row, col] });
    }
  }

  const stopGuessing = () => {
    sendMessage({ type: 'GuessingStopped' });
    setIsGuessing(false);
    setIsClueing(true);
    setClueGiven(null);
    setPartnerClueing(false);
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
          : <p>Your partner is {partnerClueing ? 'thinking up a clue...' : 'guessing!'}</p>
      }
      {props.cardState.map((row, rowNo) => (
        <div>
          {row.map(({ word, type }, colNo) => (
            <Card
              cardText={word}
              type={type.toLowerCase()}
              guessMode={isGuessing}
              guessCard={guessCard(rowNo + 1, colNo + 1)}
            />
          ))}
        </div>
      ))}
      {isClueing &&
        <ClueInput allWords={allWords} onClueGiven={onClueGiven} isFirstTurn={isFirstTurn} />
      }
      {isGuessing && hasGuessed &&
        <StopGuessingButton>
          <button className='stop-guessing' onClick={stopGuessing}>
            Stop Guessing
          </button>
        </StopGuessingButton>
      }
    </React.Fragment>
  );
};

export default Game;
