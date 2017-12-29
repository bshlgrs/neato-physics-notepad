import React from 'react';

const BuckTex = (props) => {
  const {el, ...other} = props;
  const type = el.typeStr;

  const makeChildren = (items) => items.map((x, idx) => <BuckTex {...other} key={idx} el={x} />)

  if (type == "FlexBox") {
    const style = {
      display: 'flex',
      flexDirection: el.flexDirection.dir,
      alignItems: el.alignItems.dir
    }
    return <div style={style} key={props.key}>
      {makeChildren(el.jsItems)}
    </div>
  } else if (type == "Sup") {
    return <div style={{paddingBottom: "5px", fontSize: "75%"}}>
      {makeChildren(el.jsItems)}
    </div>
  } else if (type == "Sub") {
    return <div style={{marginBottom: "-5px", fontSize: "75%"}}>
      {makeChildren(el.jsItems)}
    </div>
  } else if (type == "Fraction") {
    return <div className='fraction'>
      <div className='numerator'>{makeChildren(el.numerator.jsItems)}</div>
      <div className='denominator'>{makeChildren(el.denominator.jsItems)}</div>
    </div>;
  } else if (type == "Text") {
    return <span>{el.text}</span>;
  } else if (type == "Wrapper") {
    return <BuckTex {...other} el={el.item} />
  }
}
