import React from 'react';

import Card from './Card';

const Game = (props) => (
    props.cardState.map(row => (
        <div>
            {row.map(({ word, type }) => (
                <Card cardText={word} type={type.toLowerCase()} />
            ))}
        </div>
    ))
);

export default Game;