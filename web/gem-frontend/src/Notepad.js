import React, { Component } from 'react';
import './App.css';
import './math.css';
import Gem from './Gem';
import Immutable from 'immutable';
import BuckTex from './BuckTex';
import InfoBox from './InfoBox';
import Textarea from 'react-textarea-autosize';
import Triangle from './Triangle';

const DRAGGING = "dragging";
const DRAGGING_FROM_VAR = "dragging-from-var";
const DRAGGING_FROM_EXPR_VAR = "dragging-from-expr-var";
const GemUtils = window.GemUtils;

const getPosition = (ref) => {
  const computedStyle = ref.getBoundingClientRect();
  return {
    top: parseInt(computedStyle.top, 10),
    left: parseInt(computedStyle.left, 10)
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

const makePaddedLine = (x1, y1, x2, y2, padding) => {
  if (((x1 - x2)**2) + ((y1 - y2)**2) < (padding * 2)**2) {
    return [x1, y1, x2, y2];
  }
  const dx = x2 - x1;
  const dy = y2 - y1;
  const angle = Math.atan2(dx, dy);
  const xPadding = Math.sin(angle) * padding;
  const yPadding = Math.cos(angle) * padding;

  return [x1 + xPadding, y1 + yPadding, x2 - xPadding, y2 - yPadding];
}

const checkCollisionWithRef = (ref, pageX, pageY) => {
  const rect = ref.getBoundingClientRect();
  if (rect.left < pageX && rect.left + rect.width > pageX) {
    if (rect.top < pageY && rect.top + rect.height > pageY) {
      return true;
    }
  }
  return false;
}

class Notepad extends Component {
  constructor () {
    super();
    this.resetLocalVars();
    this.state = this.getInitialState();
    this.onMouseMoveWhileDraggingThing = this.onMouseMoveWhileDraggingThing.bind(this);
    this.onMouseMoveWhileDragging = this.onMouseMoveWhileDragging.bind(this);
    this.onMouseUp = this.onMouseUp.bind(this);
    this.onMouseUpFromVarDrag = this.onMouseUpFromVarDrag.bind(this);
    this.onMouseUpFromExprVarDrag = this.onMouseUpFromExprVarDrag.bind(this);
  }
  resetLocalVars () {
    this.varRefs = {};
    this.varPositions = {};
    this.expressionRefs = {};
    this.equationRefs = {};
    this.numberRefs = {};
    this.numberPositions = {};
    this.needsPositionRefresh = false;
  }
  reset () {
    this.resetLocalVars();
    this.props.setWorkspace(Gem.Workspace());
    this.props.setPositions(Immutable.Map());
    this.setState(this.getInitialState());
  }
  getInitialState() {
    return {
      currentAction: null,
      searchBarText: '',
      currentlySelected: { type: null, id: null }
    };
  }
  refreshStoredPositions () {
    if (!this.equationSpaceDiv) {
      return;
    }
    const parentPos = getPosition(this.equationSpaceDiv);
    Object.keys(this.varRefs).forEach((varRefString) => {
      if (this.varRefs[varRefString]) {
        const varCenter = getCenterOfElement(this.varRefs[varRefString]);
        this.varPositions[varRefString] = {
          left: varCenter.left - parentPos.left,
          top: varCenter.top - parentPos.top
        };
      }
    });

    Object.keys(this.numberRefs).forEach((numberRefString) => {
      if (this.numberRefs[numberRefString]) {
        const numCenter = getCenterOfElement(this.numberRefs[numberRefString]);
        this.numberPositions[numberRefString] = {
          left: numCenter.left - parentPos.left,
          top: numCenter.top - parentPos.top
        };
      }
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
      document.addEventListener('mousemove', this.onMouseMoveWhileDragging)
    } else if (actionFinished(DRAGGING_FROM_VAR)) {
      document.removeEventListener('mouseup', this.onMouseUpFromVarDrag);
      document.removeEventListener('mousemove', this.onMouseMoveWhileDragging);
      setTimeout(() => {
        this.refreshStoredPositions();
        this.forceUpdate();
      }, 10)
    }

    if (actionStarted(DRAGGING_FROM_EXPR_VAR)) {
      document.addEventListener('mouseup', this.onMouseUpFromExprVarDrag);
      document.addEventListener('mousemove', this.onMouseMoveWhileDragging);
    } else if (actionFinished(DRAGGING_FROM_EXPR_VAR)) {
      document.removeEventListener('mouseup', this.onMouseUpFromExprVarDrag);
      document.removeEventListener('mousemove', this.onMouseMoveWhileDragging);
    }

    // if (state.currentAction === DRAGGING || this.needsPositionRefresh) {
    this.refreshStoredPositions();
    this.needsPositionRefresh = false;
    // }
  }
  addEquation(equation) {
    const ws = this.props.workspace;
    const newEqId = ws.nextEqId;

    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    this.props.setWorkspace(ws.addEquation(equation));
    this.props.setPositions(this.props.positions.set('equation-' + newEqId, newPosition));
    this.setState({
      currentlySelected: { type: 'equation', id: newEqId }
    });
    // bleh
    setTimeout(() => { this.refreshStoredPositions(); }, 1);
  }
  addExpression(varId) {
    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    this.props.setWorkspace(this.props.workspace.addExpression(varId));
    this.props.setPositions(this.props.positions.set('expression-' + varId, newPosition));
  }
  addNumber(number) {
    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    const newNumberId = this.props.workspace.nextNumberId;
    this.props.setWorkspace(this.props.workspace.addNumber(number));
    this.props.setPositions(this.props.positions.set('number-' + newNumberId, newPosition));
    this.setState({
      currentlySelected: { type: 'number', id: newNumberId }
    });
    setTimeout(() => { this.refreshStoredPositions(); }, 10);
  }
  handleStartDrag(e, thingId, ref, selectionObject) {
    if (e.button !== 0) return;
    const rel = getRelativePositionOfEvent(e, ref, this.equationSpaceDiv);
    this.handleStartDragWithRel(e, thingId, rel, selectionObject);
  }
  handleStartDragWithRel(e, thingId, rel, selectionObject) {
    this.setState({
      currentAction: DRAGGING,
      rel: rel,
      draggedThingId: thingId,
      currentlySelected: selectionObject
    });

    e.stopPropagation();
    e.preventDefault();
  }
  onMouseUp (e) {
    this.setState({ currentAction: null });
    e.stopPropagation()
    e.preventDefault()
  }
  onMouseUpFromVarDrag (e) {
    this.setState({ currentAction: null });
    e.stopPropagation();
    e.preventDefault();
    const { pageX, pageY } = e;
    const draggedFromVarId = this.state.draggedFromVarId;
    const ws = this.props.workspace;
    const draggedOntoVarId = this.getDraggedOntoVarId(pageX, pageY);
    const dim = ws.getDimension(draggedFromVarId);

    if (draggedOntoVarId) {
      if (ws.consistentUnits(draggedFromVarId, draggedOntoVarId)) {
        this.props.setWorkspace(ws.addEquality(draggedFromVarId, draggedOntoVarId));
        this.needsPositionRefresh = true;
      }
    } else {
      const draggedOntoNumberId = this.getDraggedOntoNumberId(pageX, pageY);
      if (draggedOntoNumberId !== null) {
        const number = ws.getNumber(draggedOntoNumberId);

        if (ws.consistentUnitsWithDimension(draggedFromVarId, number.siDimension)) {
          this.props.setWorkspace(ws.attachNumberJs(draggedOntoNumberId, draggedFromVarId));
        } else {
          console.log("dimensions don't match:", number.siDimension.toString(), dim.toString());
        }
      } else {
        console.log("not dragged onto something");
      }
    }
  }
  getDraggedOntoVarId(pageX, pageY) {
    let draggedOntoVarId = null;
    Object.keys(this.varRefs).forEach((varIdStr) => {
      if (checkCollisionWithRef(this.varRefs[varIdStr], pageX, pageY)) {
        draggedOntoVarId = this.props.workspace.varIdStringToVarId(varIdStr);
      }
    });
    return draggedOntoVarId;
  }
  getDraggedOntoNumberId(pageX, pageY) {
    let draggedOntoNumberId = null;
    Object.keys(this.numberRefs).forEach((numberId) => {
      if (checkCollisionWithRef(this.numberRefs[numberId], pageX, pageY)) {
        draggedOntoNumberId = parseInt(numberId, 10);
      }
    });
    return draggedOntoNumberId;
  }
  onMouseUpFromExprVarDrag(e) {
    this.setState({ currentAction: null });
    const varToRemoveId = this.state.draggedFromVarToReplaceId;
    const exprVarId = this.state.draggedFromExprVarId;
    const draggedOntoVarId = this.getDraggedOntoVarId(e.pageX, e.pageY)
    if (draggedOntoVarId) {
      const draggedToEquation = draggedOntoVarId.eqIdx;

      console.log('attempting to rewrite');
      this.props.setWorkspace(
        this.props.workspace.rewriteExpression(exprVarId, varToRemoveId, draggedToEquation));
    } else {
      const ws =  this.props.workspace
      const draggedOntoNumberId = this.getDraggedOntoNumberId(e.pageX, e.pageY);
      if (draggedOntoNumberId !== null) {
        const number = ws.getNumber(draggedOntoNumberId);

        if (ws.consistentUnitsWithDimension(varToRemoveId, number.siDimension)) {
          this.props.setWorkspace(ws.attachNumberJs(draggedOntoNumberId, varToRemoveId));
        } else {
          console.log("dimensions don't match:", number.siDimension.toString());
        }
      } else {
        console.log("not dragged onto something");
      }
    }
  }
  onMouseMoveWhileDraggingThing (e) {
    if (this.state.currentAction !== DRAGGING) return;
    this.props.setPositions(this.props.positions.set(this.state.draggedThingId,
      Immutable.Map({
        x: e.pageX - this.state.rel.x,
        y: e.pageY - this.state.rel.y
      })
    ));

    e.stopPropagation(); e.preventDefault();
  }
  handleVariableClick(e, varId) {
    /// Start drag
    if (e.button !== 0) return;
    e.preventDefault();
    e.stopPropagation();
    const parentPos = getPosition(this.equationSpaceDiv);
    const dragPos = { x: e.pageX - parentPos.left, y: e.pageY - parentPos.top };
    this.setState({
      currentAction: DRAGGING_FROM_VAR,
      mouseDragStartPosition: dragPos,
      draggedFromVarId: varId,
      mouseDragPosition: dragPos
    })
  }
  handleExpressionVariableClick(e, exprVarId, varToRemoveId) {
    /// Start drag
    if (e.button !== 0) return;
    e.preventDefault();
    const parentPos = getPosition(this.equationSpaceDiv);
    const dragPos = { x: e.pageX - parentPos.left, y: e.pageY - parentPos.top };

    this.setState({
      currentAction: DRAGGING_FROM_EXPR_VAR,
      mouseDragStartPosition: dragPos,
      draggedFromVarToReplaceId: varToRemoveId,
      draggedFromExprVarId: exprVarId,
      mouseDragPosition: dragPos
    })
  }

  onMouseMoveWhileDragging (e) {
    const cA = this.state.currentAction;
    if (cA !== DRAGGING_FROM_VAR && cA !== DRAGGING_FROM_EXPR_VAR) return;
    const parentPos = getPosition(this.equationSpaceDiv);
    this.setState({ mouseDragPosition: { x: e.pageX - parentPos.left, y: e.pageY - parentPos.top } })
    e.stopPropagation();
    e.preventDefault();
  }
  renderVarDragLine () {
    const mouseDragStartPosition = this.state.mouseDragStartPosition;
    const mouseDragPosition = this.state.mouseDragPosition;
    return <line x1={mouseDragStartPosition.x} y1={mouseDragStartPosition.y}
            x2={mouseDragPosition.x} y2={mouseDragPosition.y}
       strokeWidth={1} stroke="black" strokeDasharray="5, 8" />;
  }
  renderVarEqualityLines () {
    const ws = this.props.workspace;

    if (!this.varPositions) {
      return null;
    }

    return ws.allVarsGroupedByEquality.map((list, idx) => {
      const positionList = list.map((varId) => {
        const pos = this.varPositions[varId];
        if (!pos) {
          setTimeout(() => this.forceUpdate(), 10);
        }
        return pos ? [pos.left, pos.top] : null;
      }).filter((x) => x);

      const numberId = ws.getNumberIdOfVar(list[0]);

      if(numberId !== null) {
        const numberPos = this.numberPositions[numberId];
        if (numberPos) {
          positionList.push([numberPos.left, numberPos.top]);
        }
      }
      const minimumSpanningTree = GemUtils.minimumSpanningTree(positionList);

      if (!minimumSpanningTree) {
        debugger;
      }

      return <g key={idx}>
        {minimumSpanningTree.map((tuple, idx) => {
          const [rawX1, rawY1, rawX2, rawY2] = tuple;
          const paddedLine = makePaddedLine(rawX1, rawY1, rawX2, rawY2, 20);
          if (paddedLine) {
            const [x1, y1, x2, y2] = paddedLine
            return <line key={idx} x1={x1} y1={y1} x2={x2} y2={y2} stroke="black" strokeDasharray="5, 8" />
          } else {
            return null;
          }
        })}
      </g>
    });
  }
  handleSearchBarSubmit () {
    const num = Gem.PhysicalNumber.parsePhysicalNumber(this.state.searchBarText.trim());
    if (num) {
      this.addNumber(num);
      this.setState({ searchBarText: '' });
      return;
    }

    const relevantEquations = this.props.library.relevantEquationIds(this.state.searchBarText);
    if (relevantEquations[0]) {
      this.addEquation(this.props.library.getByEqId(relevantEquations[0]));
      this.setState({ searchBarText: '' });
      return;
    }

    const customEquation = Gem.EquationParser.parseEquationJs(this.state.searchBarText);
    if (customEquation) {
      this.addEquation(customEquation);
      this.setState({ searchBarText: "" });
      return;
    }
  }
  deleteEquation(id) {
    this.props.setWorkspace(this.props.workspace.deleteEquation(id));
    this.setState({ currentlySelected: {type: null, id: null} });
  }
  deleteExpression(id) {
    this.props.setWorkspace(this.props.workspace.deleteExpression(id));
    this.setState({ currentlySelected: {type: null, id: null} });
  }
  deleteNumber(id) {
    this.props.setWorkspace(this.props.workspace.deleteNumber(id));
    this.setState({ currentlySelected: {type: null, id: null} });
  }
  detachNumber(id) {
    this.props.setWorkspace(this.props.workspace.detachNumber(id));
    this.setState({ currentlySelected: {type: null, id: null} });
  }
  removeEquality(varId) {
    this.props.setWorkspace(this.props.workspace.removeEquality(varId));
  }

  addTriangle () {
    const newDiagramId = this.props.workspace.diagrams.nextId;
    const newWs = this.props.workspace.addDiagram;
    this.props.setPositions(this.props.positions.set('diagram-' + newDiagramId,
      Immutable.fromJS({x: 100, y: 100})));
    this.props.setWorkspace(newWs);
  }

  render() {
    const ws = this.props.workspace;
    this.equationRefs = {};
    this.expressionRefs = {};
    this.varRefs = {};
    this.numberRefs = {};
    const currentAction = this.state.currentAction;

    return (
      <div className="main-app-div">
        <div className="equation-space" ref={(div) => { this.equationSpaceDiv = div; }}
          style={currentAction === DRAGGING_FROM_VAR ? {cursor: 'crosshair'} : {}}>
          <svg style={{position: 'absolute', left: 0, top: 0, height: "100%", width: "100%"}}>
            {(currentAction === DRAGGING_FROM_VAR || currentAction === DRAGGING_FROM_EXPR_VAR) &&
              this.renderVarDragLine()}
            {this.renderVarEqualityLines()}
          </svg>
          {ws.equationIds.map((equationId, idx) => {
            const pos = this.props.positions.get('equation-' + equationId);
            const muted = currentAction === DRAGGING_FROM_EXPR_VAR && (
              ws.possibleRewritesForExprJs(this.state.draggedFromExprVarId)
                .filter((x) => x[1] === equationId)
                .length === 0
            );

            return <div
              key={idx}
              className={"equation " + (muted ? "muted" : "")}
              style={{top: pos.get("y"), left: pos.get("x")}}
              ref={(div) => { this.equationRefs[equationId] = div; }}
              >
              <BuckTex
                onSpanMouseDown={(e) =>
                  this.handleStartDrag(e, 'equation-' + equationId, this.equationRefs[equationId],
                                      { type: 'equation', id: equationId })}
                onVarMouseDown={(e, varId) => this.handleVariableClick(e, varId)}
                onDoubleClick={(varId) => this.addExpression(varId)}
                varRef={(ref, varId) => { this.varRefs[varId] = ref; }}
                workspace={ws}
                muted={muted}
                draggedFromVarId={this.state.draggedFromVarId}
                idPrefix="variable-"
                currentAction={currentAction}
                el={ws.getEquationBuckTex(equationId)}
              />
            </div>
          })}

          {ws.expressionIds.map((exprVarId, idx) => {
            const pos = this.props.positions.get('expression-' + exprVarId);
            return <div key={idx}
                 className="expression"
                 ref={(div) => { this.expressionRefs[exprVarId] = div; }}
                 style={{top: pos.get("y"), left: pos.get("x")}}
                 >
              <BuckTex
                onSpanMouseDown={(e) =>
                  this.handleStartDrag(e, 'expression-' + exprVarId, this.expressionRefs[exprVarId],
                                      { type: 'expression', id: exprVarId })}
                onVarMouseDown={(e, varToRemoveId) => this.handleExpressionVariableClick(e, exprVarId, varToRemoveId)}
                onDoubleClick={null}
                varRef={null}
                workspace={ws}
                idPrefix={`expression-${exprVarId}-`}
                draggedFromVarId={this.state.draggedFromVarId}
                currentAction={currentAction}
                el={ws.getExpressionBuckTex(exprVarId)} />
            </div>;
          })}

          {ws.numberIds.map((numberId) => {
            const pos = this.props.positions.get('number-' + numberId);
            const number = ws.getNumber(numberId);
            const muted = currentAction === DRAGGING_FROM_VAR && (
              !ws.consistentUnitsWithDimension(this.state.draggedFromVarId, number.siDimension));

            return <div className='physical-number'
                        style={{position: 'absolute', top: pos.get("y"), left: pos.get("x"), color: (muted && 'grey')}}
                        ref={(div) => { this.numberRefs[numberId] = div; }}
                        key={numberId}>
              <BuckTex
                el={number.toBuckTex}
                onSpanMouseDown={(e) =>
                  this.handleStartDrag(e, 'number-' + numberId, this.numberRefs[numberId], { type: 'number', id: numberId })}
                />
            </div>;
          })}

          {ws.diagrams.keysJs.map((id) => {
            const triangle = ws.diagrams.getJs(id);
            const { x, y } = this.props.positions.get('diagram-' + id).toJS();
            const top = y;
            const left = x;
            return <div key={id} style={{position: "absolute", top, left}}>
              <Triangle
                triangle={triangle}
                onMouseDown={(e) => {
                  const rel = {
                    x: e.pageX - left,
                    y: e.pageY - top
                  }
                  return this.handleStartDragWithRel(e, 'diagram-' + id, rel, { type: 'triangle', id: id });
                }}
                  />
            </div>
          })}

          <div className='reset-button-div'>
            <button className="btn btn-large btn-danger" onClick={() => this.reset()} >
              Reset
            </button>

            <button className="btn btn-large btn-default" onClick={() => this.addTriangle()} >
              Add triangle
            </button>
          </div>
        </div>
        <div className="sidebar">
          <div className="description-div">
            <Textarea
              style={{width: '100%'}}
              onChange={(e) => this.props.setDescription(e.target.value)}
              value={this.props.description}
              placeholder="Add notes here" />
          </div>
          <div className='search-div' onClick={() => this.searchBarEl.focus()}>
            <input className='search-bar'
              value={this.state.searchBarText}
              onChange={(e) => { this.setState({ searchBarText: e.target.value })}}
              onKeyPress={(e) => {
                if (e.key === 'Enter') {
                  this.handleSearchBarSubmit();
                }
              }}
              ref={(el) => { this.searchBarEl = el; }}
              placeholder="Search for equations or type numbers here"/>
            {this.props.library.relevantEquationIds(this.state.searchBarText).map((eqId) => {
              const equation = this.props.library.getByEqId(eqId);
              return <div key={eqId} className='search-result'
                onClick={() => this.addEquation(equation)}>
                <BuckTex el={equation.showNaked} />
                <p>{equation.name}</p>
              </div>;
            })}
            {(() => {
              const dim = Gem.PhysicalNumber.parsePhysicalNumber(this.state.searchBarText);
              return dim ?
                <div className='physical-number'
                  onClick={() => this.handleSearchBarSubmit()}>
                  <BuckTex el={dim.toBuckTex} /></div> :
                null;
            })()}
            {(() => {
              const eq = Gem.EquationParser.parseEquationJs(this.state.searchBarText);
              return eq && <div className='search-result' onClick={() => this.addEquation(eq)}>
                <BuckTex el={eq.showNaked} />
                <p>Custom equation</p>
              </div>;
            })()}
          </div>

          {this.state.currentlySelected.type &&
            <InfoBox selectedType={this.state.currentlySelected.type}
                     selectedId={this.state.currentlySelected.id}
                     ws={this.props.workspace}
                     deleteExpression={(id) => this.deleteExpression(id)}
                     deleteEquation={(id) => this.deleteEquation(id)}
                     removeEquality={(varId) => this.removeEquality(varId)}
                     deleteNumber={(id) => this.deleteNumber(id)}
                     changeDimension={(id, dim) => this.props.setWorkspace(ws.changeDimension(id, dim))}
            />}
        </div>
      </div>
    );
  }
}

export default Notepad;
