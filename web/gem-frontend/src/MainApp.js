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

const blankState = {
  title: "",
  description: "",
  workspace: Gem.Workspace(),
  positions: Immutable.Map(),
  currentAction: null,
  creatorToken: null,
  notepadId: null,
  saving: false
}

class MainApp extends Component {
  constructor () {
    super();
    this.state = blankState;
  }
  makeJson () {
    return {
      title: this.state.title,
      description: this.state.description,
      creator_token: this.state.currentUserToken,
      content: {
        workspace: this.state.workspace.toJsObject,
        positions: this.state.positions.toJS()
      }
    }
  }
  componentWillMount () {
    let token = localStorage.getItem("currentUserToken");
    if (!token) {
      token = "gem-user-token-" + Math.random();
      localStorage.setItem("currentUserToken", token);
    }
    this.setState({
      currentUserToken: token
    });

    const pathname = window.location.pathname;
    if (pathname === "" || pathname === "/") {
      // we're making a new one
      this.setState({ creatorToken: token });
    } else {
      // todo: I think this breaks in some browsers
      const parts = pathname.split("/");
      if (parts[1] === "notepads") {
        const notepadId = parts[2];

        fetch("/api/notepads/" + notepadId, { headers: { 'Content-Type': 'application/json' } })
          .then((resp) => resp.json())
          .then((data) => {
            const notepad = data.notepad;
            this.setState({
              title: notepad.title,
              description: notepad.description,
              creatorToken: notepad.creator_token,
              notepadId: notepadId,
              positions: (notepad.content && Immutable.fromJS(notepad.content.positions)) || Immutable.Map(),
              workspace: (notepad.content && Gem.WorkspaceJs.parse(notepad.content.workspace, this.props.library)) || Gem.Workspace()
            });
          });
      }
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
        {this.state.title || "Untitled notepad"} <i className='fa fa-edit' />
      </h3>;
    }
  }
  save () {
    this.setState({ saving: true });
    if (this.state.notepadId) {
      fetch("/api/notepads/" + this.state.notepadId,
        { headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
          },
          method: "PUT",
          body: JSON.stringify({notepad: this.makeJson()})
        })
        .then(() => {
          // nothing
          this.setState({saving: false});
        });
    } else {
      this.sendCreateRequest();
    }
  }
  sendCreateRequest () {
    fetch("/api/notepads",
      { headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify({ notepad: this.makeJson() })
      })
      .then((resp) => resp.json())
      .then((data) => {
        this.setState({
          creatorToken: this.state.currentUserToken,
          notepadId: data.id,
          saving: false
        });
        window.history.pushState(null, null, "/notepads/" + data.id)
      });
  }
  saveAsCopy () {
    this.sendCreateRequest();
  }
  render () {
    const state = this.state;

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
          <button
            className="btn btn-default btn-large"
            onClick= //{() => { window.history.pushState(null, null, "/"); this.setState(blankState); }}
                       {() => { window.location.href = '/' }}
            style={{marginRight: "10px"}}>
            <i className='fa fa-plus' style={{paddingRight: "10px"}} />
            New
          </button>
          {state.creatorToken === state.currentUserToken &&
            <button
              className="btn btn-default btn-large"
              onClick={() => this.save()}
              style={{marginRight: "10px"}}>
            {state.saving ?
              <i className='fa fa-spinner fa-pulse fa-fw'
                 style={{paddingRight: "10px"}} /> :
              <i className='fa fa-save' style={{paddingRight: "10px"}} />}

            Save
          </button>}

          {state.notepadId &&
            <button
              className="btn btn-default btn-large"
              style={{marginRight: "10px"}}
              onClick={() => this.saveAsCopy()}>
            <i className="fa fa-code-fork" style={{paddingRight: "10px"}}  />
            Save as copy
          </button>}
          <button className='btn btn-info btn-large' style={{marginRight: "10px"}}
             onClick={() => this.setState({currentAction: 'showing-help'})}>
            <i className="fa fa-question" style={{paddingRight: "10px"}}  />
            Help
          </button>
          {false && <button className="btn btn-primary btn-large" style={{marginRight: "10px"}}>
            Sign up/log in
          </button>}
        </div>
      </nav>
     <Notepad
      key={window.location.href}
      library={this.props.library}
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

export default MainApp;
