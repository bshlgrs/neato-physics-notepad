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

class App extends Component {
  constructor () {
    super();
    this.state = {
      workspace: Gem.Workspace(),
      equationPositions: Immutable.Map()
    };
    this.onMouseMove = this.onMouseMove.bind(this);
    this.onMouseUp = this.onMouseUp.bind(this);
  }
  componentDidUpdate (props, state) {
    if (this.state.dragging && !state.dragging) {
      document.addEventListener('mousemove', this.onMouseMove)
      document.addEventListener('mouseup', this.onMouseUp)
    } else if (!this.state.dragging && state.dragging) {
      document.removeEventListener('mousemove', this.onMouseMove)
      document.removeEventListener('mouseup', this.onMouseUp)
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
    const computedStyle = this.equationRefs[equationId].getBoundingClientRect();
    const pos = { top: parseInt(computedStyle.top), left: parseInt(computedStyle.left) };

    const parentStyle = this.equationSpaceDiv.getBoundingClientRect();
    const parentPos = { top: parseInt(parentStyle.top), left: parseInt(parentStyle.left) };


    this.setState({
      dragging: true,
      rel: {
        x: e.pageX - pos.left + parentPos.left,
        y: e.pageY - pos.top + parentPos.top
      },
      draggedEquationId: equationId
      // TODO: say what we're dragging
    });

    e.stopPropagation();
    e.preventDefault();
  }
  onMouseUp (e) {
    this.setState({dragging: false})
    e.stopPropagation()
    e.preventDefault()
  }
  onMouseMove (e) {
    if (!this.state.dragging) return;
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
  render() {
    const ws = this.state.workspace;
    this.equationRefs = {}

    return (
      <div className="App">
        <h3>Equations</h3>

        <div className="equationSpace" ref={(div) => { this.equationSpaceDiv = div; }}>
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
                  onVarMouseDown={(e, varId) => { console.log(varId); }}
                  stuff={ws.getEquationDisplay(equationId).jsItems}
                />
              </div>
              <div style={{marginLeft: "20px"}}>({equationId})</div>
            </div>
          })}
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
