import React, { Component } from 'react';
import "./EquationEditorApp.css";
import Immutable from 'immutable';
import BuckTex from './BuckTex';
import Gem from "./Gem";

const emptyEquation = Immutable.fromJS({
  content: { eqString: "", dimensions: {}, varNames: {}, constantsUsed: [] },
  name: "",
  description: ""
});

class EquationEditorApp extends Component {
  constructor () {
    super();
    this.state = {
      newEq: emptyEquation,
      editingEq: emptyEquation,
      currentAction: null,
      editedId: null,
      equations: Immutable.Map(),
      loading: true
    }
  }
  componentWillMount () {
    fetch("/api/equations/", { headers: { 'Content-Type': 'application/json' } })
      .then((resp) => resp.json())
      .then((data) => {
        this.setState({equations: Immutable.fromJS(data.equations), loading: false});
      });
  }
  createEquation () {
    const newEq = this.state.newEq;

    fetch("/api/equations",
      { headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify({ equation: newEq })
      })
      .then((resp) => resp.json())
      .then((data) => {
        const newEqWithId = newEq.set('id', data.id).set('created_at', 'zzz'+(new Date()).getTime());
        debugger;
        this.setState({
          equations: this.state.equations.set(data.id, newEqWithId),
          newEq: emptyEquation,
          currentAction: null
        });
      });
  }
  deleteEquation (id) {
    const that = this;
    fetch("/api/equations/" + id,
      { headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "DELETE"
      })
      .then((data) => {
        debugger;
        this.setState({
          equations: this.state.equations.delete(id + "")
        })
      });
  }
  updateEquation () {
    function filterKeysToList(map, list) {
      const out = {};
      list.foreach((x) => {
        if (map[x]) {
          out[x] = map[x];
        }
      });
      return Immutable.fromJS(out);
    }

    const eq = this.state.editingEq;
    const id = this.state.editingEq.get('id');
    const { content, name } = eq.toJS();
    let { eqString, dimensions, varNames, description, constantsUsed } = content || {};
    const newEq = Gem.EquationParser.parseEquationJs(eqString || "");
    if (newEq) {
      dimensions = filterKeysToList(dimensions, newEq.varsJs);
      varNames = filterKeysToList(varNames, newEq.varsJs);
    }

    fetch("/api/equations/" + id,
      { headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "PATCH",
        body: JSON.stringify({ equation: {
          name,
          content: { eqString, dimensions, varNames, description, constantsUsed }
        }})
      })
      .then((data) => {
        this.setState({
          equations: this.state.equations.set(id + "", eq),
          currentAction: null
        })
      });
  }
  render () {
    return <div className='container'>
      <div className='row'>
        <div className='col-xs-10 col-xs-offset-1'>
          <h1>Equation editor</h1>
          {this.state.loading && <p><i className='fa fa-spinner fa-pulse' style={{paddingRight: "10px"}} /> Loading</p>}
          {this.state.equations ? this.state.equations.toList().sortBy((x) => x.get('created_at')).map((eq) => {
            if (this.state.editedId === eq.get('id') && this.state.currentAction === 'editing') {
              return <EquationEditor
                key={eq.get('id')}
                eq={this.state.editingEq}
                changeEq={(newEq) => this.setState({editingEq: newEq})}
                createText="Save"
                discardText="Discard changes"
                onCreate={() => this.updateEquation(eq.get('id'))}
                onDiscard={() => this.setState({ currentAction: null, newEq: emptyEquation })}
              />
            } else {
              return <EquationShow
                key={eq.get('id')}
                eq={eq}
                onDelete={() => this.deleteEquation(eq.get('id'))}
                startEditing={() => this.setState({ currentAction: 'editing', editedId: eq.get('id'), editingEq: eq.get('id') })}
              />;
            }

          }).toList() : null

            }
          {this.state.currentAction === "new" &&
            <EquationEditor
              eq={this.state.newEq}
              changeEq={(newEq) => this.setState({newEq: newEq})}
              createText="Create"
              discardText="Discard"
              onCreate={() => this.createEquation()}
              onDiscard={() => this.setState({ currentAction: null, newEq: emptyEquation })}
              />}
          {this.state.currentAction === null && <button className="btn btn-primary"
             onClick={() => this.setState({currentAction: 'new'})}>New</button>}
        </div>
      </div>
    </div>;
  }
}

const EquationEditor = (props) => {
  const { eq, changeEq, onCreate, onDiscard, createText, discardText } = props;
  const { content, name } = eq.toJS();
  const { eqString, dimensions, varNames, description, constantsUsed } = content || {};
  const newEq = Gem.EquationParser.parseEquationJs(eqString || "");
  // dimensions = Immutable.fromJS(dimensions);
  // varNames = Immutable.fromJS(varNames);

  return <div className="panel panel-default">
    <div className="panel-body">
      <div>
        <input placeholder="Name" value={name} onChange={(e) => changeEq(eq.set('name', e.target.value)) } /><br/>
        <input placeholder="Description" value={description || ""}
          onChange={(e) => changeEq(eq.setIn(['content', 'description'], e.target.value)) }/><br/>
        <SpaceSeparatedListInput value={constantsUsed || []}
                                 onChange={(v) => changeEq(eq.setIn(['content', 'constantsUsed'], v)) }
          placeholder="Constants used"/><br/>
        <input value={eqString} onChange={(e) => changeEq(eq.setIn(['content', 'eqString'], e.target.value)) }
          placeholder="Equation text"/>

      </div>
      {newEq && <div>
        <BuckTex el={newEq.showNaked} />
        <ul>{newEq.varsJs.filter((x) => constantsUsed.indexOf(x) === -1).map((varSym) => {
          const dim = dimensions[varSym] || "";
          const parsedDim = dim && Gem.Dimension.parseJs(dim);
          return <li key={varSym}>
            {varSym}
            <input value={varNames[varSym] || ""} placeholder="Name"
              onChange={(e) => changeEq(eq.setIn(['content', 'varNames', varSym], e.target.value))} />
            <input value={dim || ""} placeholder="Dimensions"
              onChange={(e) => changeEq(eq.setIn(['content', 'dimensions', varSym], e.target.value))} />
              {parsedDim && <BuckTex el={parsedDim.siDimension.toBuckTex} inline />}
          </li>;
        })}</ul>
      </div>}
      <div className='btn-group'>
        <button onClick={onCreate} className='btn btn-success'>{createText}</button>
        <button onClick={onDiscard} className='btn btn-danger'>{discardText}</button>
      </div>
    </div>
  </div>;
}

class SpaceSeparatedListInput extends React.Component {
  constructor (props) {
    super();
    this.state = { text: props.value.join(" ") };
  }
  render () {
    return <input value={this.state.text}
      placeholder={this.props.placeholder}
      onChange={(e) => {
      this.setState({text: e.target.value});
      this.props.onChange(e.target.value.split(" ").filter((x) => x));
    }} />
  }
}

class EquationShow extends React.PureComponent {
  render () {
    const { eq, startEditing, onDelete } = this.props;
    const { content, name } = eq.toJS();
    const { eqString, dimensions, varNames, description, constantsUsed } = content || {};

    const newEq = Gem.EquationParser.parseEquationJs(eqString || "");
    return <div className="panel panel-default">
      <div className="panel-body">
        <div>
          <p><strong>Name:</strong> {name}</p>
          <p><strong>Description:</strong> {description}</p>
          <p><strong>Constants used:</strong> {constantsUsed.join(" ")}</p>
          <p><strong>Equation text:</strong> {eqString}</p>
        </div>
        {newEq && <div>
          <BuckTex el={newEq.showNaked} />
          <ul>{newEq.varsJs.filter((x) => constantsUsed.indexOf(x) === -1).map((varSym) => {
            const dim = dimensions[varSym] || "";
            const parsedDim = dim && Gem.Dimension.parseJs(dim);
            return <li key={varSym}>
              {varSym}:&nbsp;
              <span>{varNames[varSym] || ""}</span> ::&nbsp;
              {parsedDim ? <BuckTex el={parsedDim.siDimension.toBuckTex} inline /> :
                <span>dim failed to parse: {dim}</span>}
            </li>;
          })}</ul>
        </div>}
        <div className='btn-group'>
          <button onClick={startEditing}className='btn btn-default'>Edit</button>
          <button onClick={onDelete} className='btn btn-danger'>Delete</button>
        </div>
      </div>
    </div>;
  }
}

export default EquationEditorApp;
