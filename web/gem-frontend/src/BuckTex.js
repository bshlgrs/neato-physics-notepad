import React from 'react';

const BuckTex = (props) => {
  const {el, inline, ...other} = props;
  const type = el.typeStr;
  if (inline) {
    return <div style={{display: "inline-block"}}><BuckTex {...other} el={el} /></div>;
  }

  const makeChildren = (items) => items.map((x, idx) => <BuckTex {...other} key={idx} el={x} />)

  if (type === "FlexBox") {
    const style = {
      display: 'flex',
      flexDirection: el.flexDirection.dir,
      alignItems: el.alignItems.dir
    }
    return <div // TODO: add this back in: onMouseDown={props.onSpanMouseDown && ((e) => props.onSpanMouseDown(e))}
                style={style}>
      {makeChildren(el.jsItems)}
    </div>
  } else if (type === "Sup") {
    return <div style={{marginBottom: "0.7em", fontSize: "75%"}}>
      {makeChildren(el.jsItems)}
    </div>
  } else if (type === "Sub") {
    return <div style={{marginBottom: "-0.3em", marginLeft: "-0.15em", fontSize: "75%"}}>
      {makeChildren(el.jsItems)}
    </div>
  } else if (type === "Fraction") {
    return <div className='fraction'>
      <div className='numerator'>{makeChildren(el.jsNumerator)}</div>
      <div className='denominator'>{makeChildren(el.jsDenominator)}</div>
    </div>;
  } else if (type === "Text") {
    return <span onMouseDown={props.onSpanMouseDown && ((e) => props.onSpanMouseDown(e))}>{el.text}</span>;
  } else if (type === "Wrapper") {
    return <BuckTexVariable {...other} el={el} />
  }
}

const BuckTexVariable = (props) => {
  const {el, inline, ...other} = props;
  const varId = el.data.varId;
  let color;
  if (props.currentAction === 'dragging-from-var') {
    const draggedFromVarId = props.draggedFromVarId;
    const ws = props.workspace;
    if (ws.consistentUnits(draggedFromVarId, varId)) {
      if (props.workspace.equalities.testEqual(varId, draggedFromVarId)) {
        color = "green";
      } else {
        color = "red";
      }
    } else {
      color = 'grey';
    }
  }
  if (props.muted) {
    color = 'grey';
  }

  return <span
    className="equation-var"
    style={{color: color}}
    id={props.idPrefix ? props.idPrefix + varId.toString() : null}
    onMouseDown={props.onVarMouseDown && ((e) => props.onVarMouseDown(e, varId))}
    onDoubleClick={props.onDoubleClick && ((e) => props.onDoubleClick(varId))}
    ref={props.varRef && ((ref) => { props.varRef(ref, varId)})}
    >

    <BuckTex {...other} el={el.item} onSpanMouseDown={null} />
  </span>
}

export default BuckTex;

 // } else if (name === "variableSpan") {
;
