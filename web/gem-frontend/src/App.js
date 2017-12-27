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
const DRAGGING = "dragging";
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
      positions: Immutable.Map(),
      currentAction: null
    };
    this.onMouseMoveWhileDraggingThing = this.onMouseMoveWhileDraggingThing.bind(this);
    this.onMouseMoveWhileDraggingFromVar = this.onMouseMoveWhileDraggingFromVar.bind(this);
    this.onMouseUp = this.onMouseUp.bind(this);
    this.onMouseUpFromVarDrag = this.onMouseUpFromVarDrag.bind(this);
    this.varRefs = {};
    this.varPositions = {};
    this.expressionRefs = {};
    this.equationRefs = {};
  }
  refreshVarPositions () {
    const parentPos = getPosition(this.equationSpaceDiv);
    Object.keys(this.varRefs).forEach((varRefString) => {
      const varCenter = getCenterOfElement(this.varRefs[varRefString]);
      this.varPositions[varRefString] = {
        left: varCenter.left - parentPos.left,
        top: varCenter.top - parentPos.top
      };
    });
  }
  componentDidUpdate (props, state) {
    const actionStarted = (action) => this.state.currentAction === action && state.currentAction !== action;
    const actionFinished = (action) => this.state.currentAction !== action && state.currentAction === action;

    if (actionStarted(DRAGGING)) {
      document.addEventListener('mousemove', this.onMouseMoveWhileDraggingThing)
      document.addEventListener('mouseup', this.onMouseUp)
    } else if (actionFinished(DRAGGING)) {
      document.removeEventListener('mousemove', this.onMouseMoveWhileDraggingThing)
      document.removeEventListener('mouseup', this.onMouseUp)
    }

    if (actionStarted(DRAGGING_FROM_VAR)) {
      document.addEventListener('mouseup', this.onMouseUpFromVarDrag);
      document.addEventListener('mousemove', this.onMouseMoveWhileDraggingFromVar)
    } else if (actionFinished(DRAGGING_FROM_VAR)) {
      document.removeEventListener('mouseup', this.onMouseUpFromVarDrag);
      document.removeEventListener('mousemove', this.onMouseMoveWhileDraggingFromVar);
    }

    if (state.currentAction === DRAGGING) {
      this.refreshVarPositions();
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
      positions: this.state.positions.set(newEqId, newPosition)
    });
    // bleh
    setTimeout(() => { this.refreshVarPositions() }, 1);
  }
  addExpression(varId) {
    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    this.setState({
      workspace: this.state.workspace.addExpression(varId),
      positions: this.state.positions.set(varId, newPosition)
    });
  }
  handleStartDrag(e, thingId, ref) {
    if (e.button !== 0) return;
    console.log("foo");

    this.setState({
      currentAction: DRAGGING,
      rel: getRelativePositionOfEvent(e, ref, this.equationSpaceDiv),
      draggedThingId: thingId
    });

    e.stopPropagation();
    e.preventDefault();
  }
  onMouseUp (e) {
    this.setState({currentAction: null})
    e.stopPropagation()
    e.preventDefault()
  }
  onMouseUpFromVarDrag (e) {
    this.setState({currentAction: null});
    e.stopPropagation();
    e.preventDefault();
    const { pageX, pageY } = e;
    let draggedOntoVar;
    const varRefs = this.varRefs;
    const draggedFromVarId = this.state.draggedFromVarId;
    const ws = this.state.workspace;
    const dim = ws.getDimension(draggedFromVarId);

    Object.keys(this.varRefs).forEach((varIdStr) => {
      const ref = varRefs[varIdStr];
      const rect = ref.getBoundingClientRect();
      if (rect.left < pageX && rect.left + rect.width > pageX) {
        if (rect.top < pageY && rect.top + rect.height > pageY) {
          const varId = ws.varIdStringToVarId(varIdStr);
          if (ws.getDimension(varId).toString() === dim.toString()) {
            draggedOntoVar = varId;
          }
        }
      }
    });

    if (draggedOntoVar) {
      this.setState({ workspace: ws.addEquality(draggedFromVarId, draggedOntoVar)});
    }
  }
  onMouseMoveWhileDraggingThing (e) {
    if (this.state.currentAction !== DRAGGING) return;
    this.setState({
      positions: this.state.positions.set(this.state.draggedThingId,
        Immutable.Map({
        x: e.pageX - this.state.rel.x,
        y: e.pageY - this.state.rel.y
      }))
    });
    e.stopPropagation(); e.preventDefault();
  }
  handleVariableClick(e, varId) {
    if (e.button !== 0) return;
    e.preventDefault();
    const parentPos = getPosition(this.equationSpaceDiv);
    this.setState({
      currentAction: DRAGGING_FROM_VAR,
      draggedFromVarId: varId,
      varDragPosition: { x: e.pageX - parentPos.left, y: e.pageY - parentPos.top }
    })
  }
  onMouseMoveWhileDraggingFromVar (e) {
    if (this.state.currentAction !== DRAGGING_FROM_VAR) return;
    const parentPos = getPosition(this.equationSpaceDiv);
    this.setState({varDragPosition: { x: e.pageX - parentPos.left, y: e.pageY - parentPos.top }})
    e.stopPropagation();
    e.preventDefault();
  }
  renderVarDragLine () {
    const varRef = document.getElementById(`variable-${this.state.draggedFromVarId.toString()}`)
    const varPos = getCenterOfElement(varRef);
    const parentPos = getPosition(this.equationSpaceDiv);

    return <line x1={varPos.left - parentPos.left} y1={varPos.top - parentPos.top}
            x2={this.state.varDragPosition.x} y2={this.state.varDragPosition.y}
       strokeWidth={1} stroke="black" strokeDasharray="5, 5" />;
  }
  renderVarEqualityLines () {
    // return <line x1={10} x2={30} y1={20} y2={50} strokeWidth={1} stroke="black" />;
    const ws = this.state.workspace;

    return ws.equalityListOfLists.map((list, idx) => <g key={idx}>
      {list.map((var1) =>  {
        const var1pos = this.varPositions[var1];
        return <g key={var1}>
          {list.map((var2) => {
            if (var1.toString() > var2.toString()) {
              const var2pos = this.varPositions[var2];
              return <line key={var2} x1={var1pos.left} y1={var1pos.top}
                           stroke="black" x2={var2pos.left} y2={var2pos.top}
                           strokeDasharray="5, 5" />
            } else {
              return null;
            }
          })}
        </g>;
      })}
    </g>);
  }
  render() {
    const ws = this.state.workspace;
    this.equationRefs = {};
    this.expressionRefs = {};
    this.varRefs = {};

    return (
      <div className="App">
        <h3>Equations</h3>

        <div className="equationSpace" ref={(div) => { this.equationSpaceDiv = div; }}
          style={this.state.currentAction === DRAGGING_FROM_VAR ? {cursor: 'crosshair'} : {}}>
          <svg style={{position: 'absolute', left: 0, top: 0, height: "100%", width: "100%"}}>
            {this.state.currentAction === DRAGGING_FROM_VAR &&
              this.renderVarDragLine()}
            {this.renderVarEqualityLines()}
          </svg>
          {ws.equationIds.map((equationId, idx) => {
            const pos = this.state.positions.get(equationId);

            return <div
              key={idx}
              className="equation"
              style={{top: pos.get("y"), left: pos.get("x")}}
              ref={(div) => { this.equationRefs[equationId] = div; }}
              >
              <DisplayMath
                onSpanMouseDown={(e) => this.handleStartDrag(e, equationId, this.equationRefs[equationId])}
                onVarMouseDown={(e, varId) => this.handleVariableClick(e, varId)}
                onDoubleClick={(varId) => this.addExpression(varId)}
                varRef={(ref, varId) => { this.varRefs[varId] = ref; }}
                workspace={ws}
                draggedFromVarId={this.state.draggedFromVarId}
                currentAction={this.state.currentAction}
                stuff={ws.getEquationDisplay(equationId).jsItems}
              />
            </div>
          })}

          {ws.expressionIds.map((varId, idx) => {
            const pos = this.state.positions.get(varId);

            return <div key={idx}
                 className="expression"
                 ref={(div) => { this.expressionRefs[varId] = div; }}
                 style={{top: pos.get("y"), left: pos.get("x")}}
                 >
              <DisplayMath
                onSpanMouseDown={(e) => this.handleStartDrag(e, varId, this.expressionRefs[varId])}
                onVarMouseDown={(e, varId) => {}}
                onDoubleClick={null}
                varRef={null}
                workspace={ws}
                draggedFromVarId={null}
                currentAction={this.state.currentAction}
                stuff={ws.getExpressionDisplay(varId).jsItems} />
            </div>;
          })}
        </div>

        <button onClick={() => { this.addEquation("ke_def") }}>
          Add KE equation</button>
        <button onClick={() => { this.addEquation("pe_def") }}>
          Add PE equation</button>

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

      </div>
    );
  }
}

export default App;
