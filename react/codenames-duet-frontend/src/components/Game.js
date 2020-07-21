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
  const [gameStatus, setGameStatus] = useState('ongoing');

  const endTurn = () => {
    setIsClueing(false);
    setIsGuessing(false);
    setClueGiven(null);
    setPartnerClueing(true);
  }

  const setStatus = (theRow, theCol, status) => props.cardState.map((row, rowIndex) => (
    (rowIndex === theRow - 1) ? (
      row.map((prevCard, colIndex) => (
        (colIndex === theCol - 1) ? { ...prevCard, status } : prevCard
      ))
    ) : row
  ));


  const stopGuessing = () => {
    sendMessage({ type: 'GuessingStopped' });
    setIsGuessing(false);
    setIsClueing(true);
    setClueGiven(null);
    setPartnerClueing(false);
  }

  const sendMessage = useMessageInput((received) => {
    if (received) {
      const { type, message } = received;
      switch (type) {
        case 'CardsForGame':
          // reset everything to starting state:
          setIsClueing(true);
          setIsGuessing(false);
          setClueGiven(null);
          setHasGuessed(false);
          setIsFirstTurn(true);
          setPartnerClueing(false);
          setGameStatus('ongoing');
          break;
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
            const [row, col, status] = message;
            props.updateStatuses(setStatus(row, col, status));
            switch (status) {
              case "Bystander":
                if (isGuessing) {
                  stopGuessing();
                }
                break;
              case "Assassin":
                setGameStatus('lost');
                break;
              case "Agent":
              default:
                break;
            }
          }
          break;
        case 'GuessingStoppedResponse':
          endTurn();
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

  const gameFinished = hasWon => {
    const message = hasWon ? 'Well done, you won!' : 'Sorry you lost!';
    return (
      <div>
        <p>{message}</p>
        {props.newGameButton /* TODO, get to send new message, to start new game "in place" */}
      </div>
    );
  };

  const ongoingMessage = clueGiven ? (
    <div>
      <p>Clue given by your partner:</p>
      <p>{`${clueGiven[0]} - ${clueGiven[1]}`}</p>
    </div>
  ) : isClueing
      ? null
      : <p>Your partner is {partnerClueing ? 'thinking up a clue...' : 'guessing!'}</p>

  const renderGame = () => (
    <React.Fragment>
      {props.cardState.map((row, rowNo) => (
        <div>
          {row.map(({ word, type, status }, colNo) => (
            <Card
              cardText={word}
              type={type.toLowerCase()}
              status={status}
              guessMode={isGuessing}
              guessCard={guessCard(rowNo + 1, colNo + 1)}
            />
          ))}
        </div>
      ))}
      {isClueing &&
        <ClueInput allWords={allWords} onClueGiven={onClueGiven} isFirstTurn={isFirstTurn} />
      }
      {isGuessing && hasGuessed && (gameStatus === 'ongoing') &&
        <StopGuessingButton>
          <button className='stop-guessing' onClick={stopGuessing}>
            Stop Guessing
          </button>
        </StopGuessingButton>
      }
    </React.Fragment>
  );

  return (
    <React.Fragment>
      {(gameStatus === 'ongoing') ? ongoingMessage : gameFinished(gameStatus === 'won')}
      {renderGame()}
    </React.Fragment>
  );
};

export default Game;
