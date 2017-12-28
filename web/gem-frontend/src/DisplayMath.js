import React from 'react';

const DisplayMath = (props) => {
  const {stuff, ...other} = props;

  return <div className='math-row'>
    {stuff.map((x, idx) => <DisplayMathElement {...other} el={x} key={idx} />)}
  </div>;
}

const DisplayMathElement = (props) => {
  const {el, ...other} = props;
  const name = el.name;

  if (name === "span") {
    // TODO: Does it work for me to just set onClick to be `{props.onSpanClick}`
    // rather than wrapping it?
    return <span onMouseDown={props.onSpanMouseDown && ((e) => props.onSpanMouseDown(e))}>{el.str}</span>;
  } else if (name === "variableSpan") {
    const varId = el.varId;
    let color;
    if (props.currentAction === 'dragging-from-var') {
      const draggedFromVarId = props.draggedFromVarId
      if (props.workspace.getDimension(draggedFromVarId).equalUnits(props.workspace.getDimension(varId))) {
        if (draggedFromVarId.toString() === varId.toString() ||
            props.workspace.equalities.testEqual(varId, draggedFromVarId)) {
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
      {el.jsEls.map((x, idx) =>
      <DisplayMathElement {...other} key={idx} el={x} onSpanMouseDown={() => {}} />)}
    </span>;
  } else if (name === "sup") {
    return <sup>{el.jsInner.map((x, idx) => <DisplayMathElement {...other} key={idx} el={x} />)}</sup>;
  } else if (name === "sub") {
    return <sub>{el.jsInner.map((x, idx) => <DisplayMathElement {...other} key={idx} el={x} />)}</sub>;
  } else if (name === "fraction") {
    return <div className='fraction'>
      <div className='numerator'><DisplayMath {...other} stuff={el.numerator.jsItems} /></div>
      <div className='denominator'><DisplayMath {...other} stuff={el.denominator.jsItems} /></div>
    </div>;
  } else if (name === "box") {
    return <Box {...other} stuff={el.stuff.jsItems} />;
  } else {
    debugger;
  }
}

const Box = (props) => {
  const {stuff, ...other} = props;
  return <div className='aligned-row'>
    {stuff.map((x, idx) => <DisplayMathElement {...other} el={x} key={idx} />)}
  </div>;
}

export default DisplayMath;
