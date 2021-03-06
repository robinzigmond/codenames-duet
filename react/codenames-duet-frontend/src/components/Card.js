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
      cursor: ${props => props.guessMode
    && ['open', 'Bystander-theyGuessed'].includes(props.status)
    ? 'pointer'
    : 'normal'};
      position: relative;

      .indicator {
        position: absolute;
        top: 0;
        right: 0;
        width: ${props => props.status.startsWith('Bystander-') ? '30px' : '100%'};
        height: ${props => props.status.startsWith('Bystander-') ? '30px' : '100%'};
        background-color: ${props => props.status.startsWith('Bystander-') ? '#ccc' : 'transparent'};
        ${props => {
    const isHalfBystander = props.status.startsWith('Bystander-');
    if (isHalfBystander) {
      return `border-${props.status.endsWith('iGuessed') ? 'bottom' : 'top'}: 2px solid #0a0a0a;`;
    }
    else {
      return `border: 5px solid ${props.status === 'Assassin' ? '#030303' :
        props.status === 'Agent' ? '#0c5008' : '#ccc'};`
    }
  }};
      }
  }
`;

const Card = (props) => (
  <CardStyle
    type={props.type}
    guessMode={props.guessMode}
    onClick={props.guessCard}
    status={props.status}
  >
    {(props.status !== 'open') && <div className="indicator"></div>}
    <p>{props.cardText}</p>
  </CardStyle>
);

export default Card;