import React, { useState } from 'react';
import styled from 'styled-components';

import useMessageInput from '../hooks/useMessageInput';

const InputStyle = styled.div`
  & button.clue {
    width: auto;
    height: 40px;
    padding: 5px 20px;
  }

  & input {
    margin-right: 20px;
    margin-left: 10px;
    font-size: 18px;
    height: 40px;
  }

  & input[type="text"] {
    width: 150px;
  }

  & input[type="number"] {
    width: 60px;
  }

  & .clue-error {
    color: #cc041b;
    font-weight: bold;
  }
`;

const ClueInput = (props) => {
  const [isFirstTurn, setIsFirstTurn] = useState(true);
  const [clueWord, setClueWord] = useState('');
  const [clueNumber, setClueNumber] = useState('');
  const [clueError, setClueError] = useState('');

  const updateClue = (e) => { setClueWord(e.target.value.toUpperCase()); }

  const updateNumber = (e) => { setClueNumber(e.target.value); }

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

  const onSubmitGuess = () => {
    if (props.allWords.includes(clueWord.toUpperCase())) {
      setClueError('you can\'t guess a word which is already on the board!');
    }
    else if (Number.isNaN(clueNumber) || +clueNumber < 1) {
      setClueError('you must guess a positive number!');
    }
    else {
      setClueError('');
      setIsFirstTurn(false);
      sendMessage({
        type: 'ClueGiven',
        message: [clueWord, +clueNumber]
      });
      props.onClueGiven();
    }
  }

  return (
    <InputStyle>
      {isFirstTurn ? (
        <React.Fragment>
          <p><strong>As it's the first turn, your partner is also able to give a clue!</strong></p>
          <p>
            <strong>
              You might want to discuss with them who should go first
              - or perhaps just try to be quickest...
            </strong>
          </p>
        </React.Fragment>
      ) : <p><strong>Enter your clue:</strong></p>}
      {clueError && <p class="clue-error">{clueError}</p>}
      <label htmlFor="clue-word">Clue:</label>
      <input id="clue-word" type="text" onChange={updateClue} value={clueWord} />
      <label htmlFor="clue-number">Number of words:</label>
      <input id="clue-number" type="number" onChange={updateNumber} value={clueNumber} />
      <button className="clue" type="submit" onClick={onSubmitGuess}>Submit Clue!</button>
    </InputStyle>
  );
}

export default ClueInput;