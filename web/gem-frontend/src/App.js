import React, { Component } from 'react';
import './App.css';
import Gem from './Gem';
import katex from 'katex';
import 'katex/dist/katex.css';

const latex = (latex, displayMode) => {
  try {
    return <span
     dangerouslySetInnerHTML={{__html: katex.renderToString(latex, {displayMode: displayMode})}}/>;
  } catch (e) {
    return <span>oh dear: {latex}</span>
  }
};

class App extends Component {
  constructor () {
    super();
    this.state = {
      workspace: Gem.Workspace()
    };
  }
  setWs(newWs) {
    this.setState({ workspace: newWs });
  }
  showVar(varId) {
    return latex(this.state.workspace.showVar(varId));
  }
  addEquation(eqId) {
    this.setWs(this.state.workspace.addEquation(Gem.EquationLibrary.getByEqId(eqId)));
  }
  render() {
    const ws = this.state.workspace;
    const toJs = Gem.ToJS.convert;


    return (
      <div className="App">
        <h3>Equations</h3>

        {ws.equationIds.map((equationId, idx) =>
          <div key={idx} className="equation">
            <div>{latex(ws.showEquation(equationId), true)} </div>
            <div style={{marginLeft: "20px"}}>({equationId})</div>
          </div>
        )}

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
          <div key={idx} className="expression">{latex(ws.showExpression(varId), true)}
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
