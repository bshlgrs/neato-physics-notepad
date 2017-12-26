import React, { Component } from 'react';
import './App.css';
import Gem from './Gem';
import 'katex/dist/katex.css';
import Immutable from 'immutable';
import DisplayMath from './DisplayMath';

// const latex = (latex, displayMode) => {
//   try {
//     return <span
//      dangerouslySetInnerHTML={{__html: katex.renderToString(latex, {displayMode: displayMode})}}/>;
//   } catch (e) {
//     return <span>oh dear: {latex}</span>
//   }
// };

const danger = (str) => <span dangerouslySetInnerHTML={{__html: str}} />;
const DRAGGING_EQUATION = "dragging-equation";
const DRAGGING_FROM_VAR = "dragging-from-var";

const getPosition = (ref) => {
  const computedStyle = ref.getBoundingClientRect();
  return {
    top: parseInt(computedStyle.top),
    left: parseInt(computedStyle.left)
  };
}

const getCenterOfElement = (ref) => {
  const computedStyle = ref.getBoundingClientRect();
  return {
    left: computedStyle.left + 0.5 * computedStyle.width,
    top: computedStyle.top + 0.5 * computedStyle.height
  }
}

const getRelativePositionOfEvent = (e, ref, parentRef) => {
  const pos = getPosition(ref);
  const parentPos = getPosition(parentRef);
  return {
    x: e.pageX - pos.left + parentPos.left,
    y: e.pageY - pos.top + parentPos.top
  }
}

class App extends Component {
  constructor () {
    super();
    this.state = {
      workspace: Gem.Workspace(),
      equationPositions: Immutable.Map(),
      currentAction: null
    };
    this.onMouseMoveWhileDraggingEquation = this.onMouseMoveWhileDraggingEquation.bind(this);
    this.onMouseMoveWhileDraggingFromVar = this.onMouseMoveWhileDraggingFromVar.bind(this);
    this.onMouseUp = this.onMouseUp.bind(this);
    this.varRefs = {};
  }
  componentDidUpdate (props, state) {
    const actionStarted = (action) => this.state.currentAction === action && state.currentAction !== action;
    const actionFinished = (action) => this.state.currentAction !== action && state.currentAction === action;

    if (actionStarted(DRAGGING_EQUATION)) {
      document.addEventListener('mousemove', this.onMouseMoveWhileDraggingEquation)
      document.addEventListener('mouseup', this.onMouseUp)
    } else if (actionFinished(DRAGGING_EQUATION)) {
      document.removeEventListener('mousemove', this.onMouseMoveWhileDraggingEquation)
      document.removeEventListener('mouseup', this.onMouseUp)
    }

    if (actionStarted(DRAGGING_FROM_VAR)) {
      document.addEventListener('mouseup', this.onMouseUp);
      document.addEventListener('mousemove', this.onMouseMoveWhileDraggingFromVar)
    } else if (actionFinished(DRAGGING_FROM_VAR)) {
      document.removeEventListener('mouseup', this.onMouseUp);
      document.removeEventListener('mousemove', this.onMouseMoveWhileDraggingFromVar);
    }
  }
  setWs(newWs) {
    this.setState({ workspace: newWs });
  }
  showVar(varId) {
    return danger(this.state.workspace.showVar(varId));
  }
  addEquation(eqId) {
    const ws = this.state.workspace;
    const newWs = ws.addEquation(Gem.EquationLibrary.getByEqId(eqId));
    const newEqId = newWs.lastEqId;

    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    this.setState({
      workspace: newWs,
      equationPositions: this.state.equationPositions.set(newEqId, newPosition)
    });
  }
  handleEquationMouseDown(e, equationId) {
    if (e.button !== 0) return;

    this.setState({
      currentAction: DRAGGING_EQUATION,
      rel: getRelativePositionOfEvent(e, this.equationRefs[equationId], this.equationSpaceDiv),
      draggedEquationId: equationId
      // TODO: say what we're dragging
    });

    e.stopPropagation();
    e.preventDefault();
  }
  onMouseUp (e) {
    this.setState({currentAction: null})
    e.stopPropagation()
    e.preventDefault()
  }
  onMouseMoveWhileDraggingEquation (e) {
    if (this.state.currentAction !== DRAGGING_EQUATION) return;
    this.setState({
      equationPositions: this.state.equationPositions.set(this.state.draggedEquationId,
        Immutable.Map({
        x: e.pageX - this.state.rel.x,
        y: e.pageY - this.state.rel.y
      }))
    });
    e.stopPropagation();
    e.preventDefault();
  }
  handleVariableClick(e, varId) {
    if (e.button !== 0) return;
    // const pos = getPosition(this.varRefs[varId]);
    // const parentPos = getPosition(this.equationSpaceDiv);
    const varRef = document.getElementById(`variable-${varId.toString()}`)

    e.preventDefault();

    const pos = getPosition(varRef);
    const parentPos = getPosition(this.equationSpaceDiv);
    const rel = {
      x: e.pageX - pos.left + parentPos.left,
      y: e.pageY - pos.top + parentPos.top
    }

    this.setState({
      currentAction: DRAGGING_FROM_VAR,
      draggedFromVarId: varId,
      rel: rel,
      varDragPosition: { x: e.pageX - rel.x, y: e.pageY - rel.y }
    })
  }
  onMouseMoveWhileDraggingFromVar (e) {
    // console.log(e);
    if (this.state.currentAction !== DRAGGING_FROM_VAR) return;
    console.log("dragging from var");
    this.setState({ varDragPosition: {
      x: e.pageX - this.state.rel.x,
      y: e.pageY - this.state.rel.y
    }});
    e.stopPropagation();
    e.preventDefault();
  }
  renderVarDragLine () {
    const varRef = document.getElementById(`variable-${this.state.draggedFromVarId.toString()}`)
    const varPos = getCenterOfElement(varRef);
    const parentPos = getPosition(this.equationSpaceDiv);

    return [
      // <p key={1} style={{position: 'absolute', 'top': varPos.top - parentPos.top, 'left': varPos.left - parentPos.left}}>
      //   lol
      // </p>,
      // <p key={2} style={{position: 'absolute', 'top': this.state.varDragPosition.top, 'left': this.state.varDragPosition.left}}>
      //   lol2
      // </p>,
      <svg key={3} style={{position: 'absolute', left: 0, top: 0, height: "100%", width: "100%"}}>
        <line x1={varPos.left - parentPos.left} y1={varPos.top - parentPos.top}
              x2={this.state.varDragPosition.x} y2={this.state.varDragPosition.y}
         strokeWidth="2" stroke="black" />
      </svg>
    ]
  }
  render() {
    const ws = this.state.workspace;
    this.equationRefs = {};

    return (
      <div className="App">
        <h3>Equations</h3>

        <div className="equationSpace" ref={(div) => { this.equationSpaceDiv = div; }}
          style={this.state.currentAction === DRAGGING_FROM_VAR ? {cursor: 'crosshair'} : {}}>
          {ws.equationIds.map((equationId, idx) => {
            const pos = this.state.equationPositions.get(equationId);

            return <div
              key={idx}
              className="equation"
              style={{top: pos.get("y"), left: pos.get("x")}}
              >
              <div ref={(div) => { this.equationRefs[equationId] = div; }} >
                <DisplayMath
                  onSpanMouseDown={(e) => this.handleEquationMouseDown(e, equationId)}
                  onVarMouseDown={(e, varId) => this.handleVariableClick(e, varId)}
                  // varRef={(ref, varId) => { varRefs[varId] = ref; }}
                  workspace={ws}
                  draggedFromVarId={this.state.draggedFromVarId}
                  currentAction={this.state.currentAction}
                  stuff={ws.getEquationDisplay(equationId).jsItems}
                />
              </div>
              <div style={{marginLeft: "20px"}}>({equationId})</div>
            </div>
          })}

          {this.state.currentAction === DRAGGING_FROM_VAR &&
            this.renderVarDragLine()}
        </div>

        <button onClick={() => { this.addEquation("ke_def") }}>
          Add KE equation</button>
        <button onClick={() => { this.addEquation("pe_def") }}>
          Add PE equation</button>

        <h3>Equalities</h3>
        {ws.equalityListOfLists.map((list, idx) =>
          <p key={idx}>{list.map((varId, varIdIdx) => {
            if (varIdIdx === list.length - 1) {
              return <span key={varIdIdx}>
              {this.showVar(varId)}
              </span>
            } else {
              return <span key={varIdIdx}>
                {this.showVar(varId)}
                =
              </span>;
            }
          })}</p>
        )}

        {ws.addableEqualitiesJs.map((tuple, idx) =>
          <button
            key={idx}
            onClick={() => this.setWs(ws.addEquality(tuple[0], tuple[1]))}>
            Set {this.showVar(tuple[0])} = {this.showVar(tuple[1])}
          </button>
        )}

        <h3>Expressions</h3>

        {ws.expressionIds.map((varId, idx) =>
          <div key={idx} className="expression">{danger(ws.showExpression(varId), true)}
            {ws.possibleRewritesForExprJs(varId).map((rewrite, idx) =>
              <button key={idx}
                onClick={() => this.setWs(ws.rewriteExpression(varId, rewrite[0], rewrite[1]))}>
                Sub in equation {rewrite[1]} to replace {this.showVar(rewrite[0])}
              </button>
            )}
            <button onClick={() => this.setWs(ws.deleteExpression(varId))}>Delete</button>
          </div>
        )}

        {ws.addableExpressionIds.map((x, idx) =>
          <button key={idx} onClick={() => this.setWs(ws.addExpression(x))}>
            Add {this.showVar(x)}
          </button>
        )}
      </div>
    );
  }
}

export default App;
