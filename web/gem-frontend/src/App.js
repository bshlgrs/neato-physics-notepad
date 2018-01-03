import React, { Component } from 'react';
import './App.css';
import Immutable from 'immutable';
import Notepad from './Notepad';
import Gem from './Gem';
import 'font-awesome/css/font-awesome.min.css';
import '../node_modules/bootstrap/dist/css/bootstrap.min.css';
import ReactModal from 'react-modal';
import Help from "./Help"

ReactModal.setAppElement("#root")

class App extends Component {
  constructor () {
    super();
    this.state = {
      title: "",
      description: "",
      workspace: Gem.Workspace(),
      positions: Immutable.Map(),
      currentAction: null
    }
  }
  changeTitle(newTitle) {
    this.setState({ title: newTitle });
  }
  renderTitle () {
    if (this.state.currentAction === "editing-title") {
      return <div>
        <input className="title-input" value={this.state.title} onChange={(e) => this.changeTitle(e.target.value)}
          onBlur={() => this.setState({ currentAction: null })} ref={(d) => d && d.focus()}/>
      </div>
    } else {
      return <h3 style={{margin: 0}} onClick={() => this.setState({currentAction: 'editing-title'})}>
        {this.state.title || "Untitled page"} <i className='fa fa-edit' /> by Buck
      </h3>;
    }
  }
  render () {
    return <div className="gem-container">
      <ReactModal
           isOpen={this.state.currentAction === "showing-help"}
           contentLabel="Minimal Modal Example"
           onRequestClose={() => this.setState({currentAction: null})}
           className="mymodal"
      >
        <Help />

      </ReactModal>
      <nav className="header">
        <div className="logo">
          <h3 style={{margin: 0}}><a href="http://shlegeris.com">Buck</a>'s neato physics notepad</h3>
        </div>
        {this.renderTitle()}

        <div className="pull-right">
          <button className="btn btn-default btn-large" style={{marginRight: "10px"}}>
            <i className='fa fa-save' style={{paddingRight: "10px"}} />
            Save
          </button>

          <button className="btn btn-default btn-large" style={{marginRight: "10px"}}>
            <i className="fa fa-code-fork" style={{paddingRight: "10px"}}  />
            Fork
          </button>
          <button className='btn btn-info btn-large' style={{marginRight: "10px"}}
             onClick={() => this.setState({currentAction: 'showing-help'})}>
            <i className="fa fa-question" style={{paddingRight: "10px"}}  />
            Help
          </button>
          <button className="btn btn-primary btn-large" style={{marginRight: "10px"}}>
            Sign up/log in
          </button>
        </div>
      </nav>
     <Notepad
      workspace={this.state.workspace}
      setWorkspace={(ws) => this.setState({workspace: ws})}
      positions={this.state.positions}
      setPositions={(positions) => this.setState({positions: positions})}
      title={this.state.title}
      description={this.state.description}
      setDescription={(description) => this.setState({description: description})} />
    </div>
  }
}

export default App;
