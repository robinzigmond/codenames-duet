import React, { useState, useEffect } from 'react';
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

// TODO:
// - add "sudden death" turn (rather than autoloss) when turns run out
// - make turns and allowed incorrect guesses configurable at start of game
// (by both players?)
// - allow players to give names
// - show game "log"
// - improve colour contrast! Maybe use arrows rather than borders to indicate which side guessed a bystander?
// - somehow restrict clues of "related words?" [probably have to hardcode]
const Game = (props) => {
  const turnsInGame = 9; // hardcoded, later allow to configure at start of game
  const [turnsLeft, setTurnsLeft] = useState(turnsInGame);
  const [isClueing, setIsClueing] = useState(true);
  const [isGuessing, setIsGuessing] = useState(false);
  const [clueGiven, setClueGiven] = useState(null);
  const [hasGuessed, setHasGuessed] = useState(false);
  const [isFirstTurn, setIsFirstTurn] = useState(true);
  const [partnerClueing, setPartnerClueing] = useState(false);
  const [gameStatus, setGameStatus] = useState('ongoing');
  const [hasCluesToGive, setHasCluesToGive] = useState(true);

  const guessedAgents = props.cardState.reduce((total, rowState) =>
    total + rowState.reduce((rowTotal, card) => rowTotal + +(card.status === 'Agent')
      , 0)
    , 0);

  useEffect(() => {
    if (guessedAgents === 15) {
      setGameStatus('won');
    }
  }, [guessedAgents]);

  useEffect(() => {
    if (turnsLeft == 0) {
      setGameStatus('lost');
    }
  }, [turnsLeft]);

  useEffect(() => {
    if (props.cardState.length && hasCluesToGive) {
      for (let row = 0; row < 5; row++) {
        for (let col = 0; col < 5; col++) {
          const card = props.cardState[row][col];
          if (card.type === 'Agent' && card.status === 'open') {
            return;
          }
        }
      }
      setHasCluesToGive(false);
    }
  }, [props.cardState, hasCluesToGive]);

  useEffect(() => {
    if (!hasCluesToGive && isClueing) {
      endTurn(false);
      sendMessage({ type: 'AllMyCardsGuessed' });
    }
  }, [hasCluesToGive, isClueing])

  const isGuessable = cardObj => ['open', 'Bystander-theyGuessed'].includes(cardObj.status);

  const endTurn = (turnHappened = true) => {
    setIsClueing(false);
    setIsGuessing(false);
    setClueGiven(null);
    setPartnerClueing(true);
    if (turnHappened) {
      setTurnsLeft(turns => turns - 1);
    }
  }

  const setStatus = (theRow, theCol, status) => props.cardState.map((row, rowIndex) => (
    (rowIndex === theRow - 1) ? (
      row.map((prevCard, colIndex) => (
        (colIndex === theCol - 1) ? { ...prevCard, status } : prevCard
      ))
    ) : row
  ));

  const stopGuessing = (turnHappened = true) => {
    setIsGuessing(false);
    setIsClueing(true);
    setClueGiven(null);
    setPartnerClueing(false);
    if (turnHappened) {
      sendMessage({ type: 'GuessingStopped' });
      setTurnsLeft(turns => turns - 1);
    }
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
          setTurnsLeft(turnsInGame);
          setHasCluesToGive(true);
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
            let statusToSend = (status === 'Bystander')
              ? `Bystander-${isGuessing ? 'iGuessed' : 'theyGuessed'}`
              : status;
            if (statusToSend.startsWith('Bystander')
              && props.cardState[row - 1][col - 1].status.startsWith('Bystander')) {
              statusToSend = 'Bystander';
            }
            props.updateStatuses(setStatus(row, col, statusToSend));
            switch (status) {
              case 'Bystander':
                if (isGuessing) {
                  stopGuessing();
                }
                break;
              case 'Assassin':
                setGameStatus('lost');
                break;
              case 'Agent':
              default:
                break;
            }
          }
          break;
        case 'GuessingStoppedResponse':
          endTurn();
          break;
        case 'AllMyCardsGuessedResponse':
          stopGuessing(false);
          break;
        default:
          break;
      }
    }
  });

  const getWord = cardObj => cardObj.word;

  const allWords = props.cardState
    .reduce((all, row) => [
      ...all,
      ...row.filter(cardObj => !(['Agent', 'Bystander'].includes(cardObj.status)))
        .map(getWord)
    ], []);

  const onClueGiven = () => {
    setIsFirstTurn(false);
    setIsClueing(false);
  }

  const guessCard = (row, col) => () => {
    if (clueGiven && isGuessable(props.cardState[row - 1][col - 1])) {
      sendMessage({ type: 'CardGuessed', message: [row, col] });
    }
  }

  const gameFinished = hasWon => {
    const message = hasWon ? 'Well done, you won!' : 'Sorry you lost!';
    return (
      <div>
        <p>{message}</p>
        {props.newGameButton}
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
      <p>Turns Left: {turnsLeft}</p>
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
