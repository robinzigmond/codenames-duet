import React from 'react';

import Card from './Card';

const Game = (props) => (
    props.cards.map(row => (
        <div>
            {row.map(cardText => (
                <Card cardText={cardText} />
            ))}
        </div>
    ))
);

export default Game;