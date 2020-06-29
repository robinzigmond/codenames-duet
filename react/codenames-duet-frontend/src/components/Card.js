import React from 'react';
import styled from 'styled-components';

const CardStyle = styled.div`
  & {
      display: inline-flex;
      justify-content: center;
      align-items: center;
      margin: 10px;
      width: 150px;
      height: 65px;
      background-color: ${props => props.type === 'agent' ? '#23f018' :
    props.type === 'assassin' ? '#0a0a0a' : '#ccb43b'};
      font-size: 18px;
      font-weight: bold;
      color: ${props => props.type === 'assassin' ? '#f5f0f0' : '#2a2952'};
      letter-spacing: 0.5px;
  }
`;

const Card = (props) => (
  <CardStyle type={props.type}>
    <p>{props.cardText}</p>
  </CardStyle>
);

export default Card;