import React from 'react';

const DisplayMath = (props) => {
  const {stuff, ...other} = props;

  return <span>{stuff.map((x, idx) => <DisplayMathElement {...other} el={x} key={idx} />)}</span>;
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
    // console.log(props.draggedFromVarId);
    // console.log(varId);
    let color;
    if (props.currentAction === 'dragging-from-var') {
      if (props.workspace.getDimension(props.draggedFromVarId) === props.workspace.getDimension(varId)) {
        if (props.draggedFromVarId.toString() === varId.toString()) {
          color = "green";
        } else {
          color = "red";
        }
      } else {
        color = 'grey';
      }
    } else {

    }

    return <span
      className="equation-var"
      style={{color: color}}
      id={props.idPrefix + varId.toString()}
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
  } else {
    debugger;
  }
}

export default DisplayMath;
