import React, { Component } from 'react';
import './App.css';
import Gem from './Gem';
import Immutable from 'immutable';
import DisplayMath from './DisplayMath';

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
    return null;
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

class App extends Component {
  constructor () {
    super();
    this.state = {
      workspace: Gem.Workspace(),
      positions: Immutable.Map(),
      currentAction: null,
      searchBarText: '',
      currentlySelected: { type: null, id: null }
    };
    this.onMouseMoveWhileDraggingThing = this.onMouseMoveWhileDraggingThing.bind(this);
    this.onMouseMoveWhileDragging = this.onMouseMoveWhileDragging.bind(this);
    this.onMouseUp = this.onMouseUp.bind(this);
    this.onMouseUpFromVarDrag = this.onMouseUpFromVarDrag.bind(this);
    this.onMouseUpFromExprVarDrag = this.onMouseUpFromExprVarDrag.bind(this);
    this.varRefs = {};
    this.varPositions = {};
    this.expressionRefs = {};
    this.equationRefs = {};
    this.numberRefs = {};
    this.numberPositions = {};
  }
  refreshStoredPositions () {
    const parentPos = getPosition(this.equationSpaceDiv);
    Object.keys(this.varRefs).forEach((varRefString) => {
      const varCenter = getCenterOfElement(this.varRefs[varRefString]);
      this.varPositions[varRefString] = {
        left: varCenter.left - parentPos.left,
        top: varCenter.top - parentPos.top
      };
    });

    Object.keys(this.numberRefs).forEach((numberRefString) => {
      const numCenter = getCenterOfElement(this.numberRefs[numberRefString]);
      this.numberPositions[numberRefString] = {
        left: numCenter.left - parentPos.left,
        top: numCenter.top - parentPos.top
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
      document.addEventListener('mousemove', this.onMouseMoveWhileDragging)
    } else if (actionFinished(DRAGGING_FROM_VAR)) {
      document.removeEventListener('mouseup', this.onMouseUpFromVarDrag);
      document.removeEventListener('mousemove', this.onMouseMoveWhileDragging);
    }

    if (actionStarted(DRAGGING_FROM_EXPR_VAR)) {
      document.addEventListener('mouseup', this.onMouseUpFromExprVarDrag);
      document.addEventListener('mousemove', this.onMouseMoveWhileDragging);
    } else if (actionFinished(DRAGGING_FROM_EXPR_VAR)) {
      document.removeEventListener('mouseup', this.onMouseUpFromExprVarDrag);
      document.removeEventListener('mousemove', this.onMouseMoveWhileDragging);
    }

    if (state.currentAction === DRAGGING) {
      this.refreshStoredPositions();
    }
  }
  addEquation(eqId) {
    const ws = this.state.workspace;
    const newWs = ws.addEquation(Gem.EquationLibrary.getByEqId(eqId));
    const newEqId = newWs.lastEqId;

    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    this.setState({
      workspace: newWs,
      positions: this.state.positions.set('equation-' + newEqId, newPosition),
      currentlySelected: { type: 'equation', id: newEqId }
    });
    // bleh
    setTimeout(() => { this.refreshStoredPositions(); }, 1);
  }
  addExpression(varId) {
    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    this.setState({
      workspace: this.state.workspace.addExpression(varId),
      positions: this.state.positions.set('expression-' + varId, newPosition)
    });
  }
  addNumber(number) {
    const newPosition = Immutable.Map({x: Math.random() * 300, y: Math.random() * 300});
    const newWs = this.state.workspace.addNumber(number);
    const newNumberId = newWs.lastNumberId;
    this.setState({
      workspace: newWs,
      positions: this.state.positions.set('number-' + newNumberId, newPosition)
    });

  }
  handleStartDrag(e, thingId, ref, selectionObject) {
    if (e.button !== 0) return;

    this.setState({
      currentAction: DRAGGING,
      rel: getRelativePositionOfEvent(e, ref, this.equationSpaceDiv),
      draggedThingId: thingId,
      currentlySelected: selectionObject
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
    const draggedFromVarId = this.state.draggedFromVarId;
    const ws = this.state.workspace;
    const draggedOntoVarId = this.getDraggedOntoVarId(pageX, pageY);
    const dim = ws.getDimension(draggedFromVarId);

    if (draggedOntoVarId) {
      if (ws.getDimension(draggedOntoVarId).toString() === dim.toString()) {
        this.setState({ workspace: ws.addEquality(draggedFromVarId, draggedOntoVarId)});
      }
    } else {
      const draggedOntoNumberId = this.getDraggedOntoNumberId(pageX, pageY);
      if (draggedOntoNumberId !== null) {
        const number = ws.getNumber(draggedOntoNumberId);

        if (number.dimension.equalUnits(dim)) {
          this.setState({ workspace: ws.attachNumberJs(draggedOntoNumberId, draggedFromVarId) });
        } else {
          console.log("dimensions don't match:", number.dimension.toString(), dim.toString());
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
        draggedOntoVarId = this.state.workspace.varIdStringToVarId(varIdStr);
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
      if (draggedToEquation) {
        // TODO: check dragged-to equation is legit;
        this.setState({ workspace: this.state.workspace.rewriteExpression(exprVarId, varToRemoveId, draggedToEquation) })
      }
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
    /// Start drag
    if (e.button !== 0) return;
    e.preventDefault();
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
    this.setState({mouseDragPosition: { x: e.pageX - parentPos.left, y: e.pageY - parentPos.top }})
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
    const ws = this.state.workspace;

    if (!this.varPositions) {
      return null;
    }

    return ws.allVarsGroupedByEquality.map((list, idx) => {
      const positionList = list.map((varId) => {
        const pos = this.varPositions[varId];
        return pos ? [pos.left, pos.top] : null;
      }).filter((x) => x);

      const numberId = ws.getNumberIdOfVar(list[0]);

      if(numberId !== null) {
        const numberPos = this.numberPositions[numberId];
        positionList.push([numberPos.left, numberPos.top]);
      }
      const minimumSpanningTree = GemUtils.minimumSpanningTree(positionList);

      if (!minimumSpanningTree) {
        debugger;
      }

      return <g key={idx}>
        {minimumSpanningTree.map((tuple, idx) => {
          const [rawX1, rawY1, rawX2, rawY2] = tuple;
          const paddedLine = makePaddedLine(rawX1, rawY1, rawX2, rawY2, 15);
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
    const num = Gem.Dimension.parsePhysicalNumber(this.state.searchBarText.trim());
    if (num) {
      this.addNumber(num);
      this.setState({ searchBarText: '' });
    }
  }
  deleteEquation(id) {
    this.setState({ workspace: this.state.workspace.deleteEquation(id),
                    currentlySelected: {type: null, id: null} });
  }
  renderCurrentlySelectedInfo () {
    const selectedType = this.state.currentlySelected.type;
    const selectedId = this.state.currentlySelected.id;
    const ws = this.state.workspace;
    if (selectedType === "equation") {
      const equation = ws.getEquation(selectedId);

      return <div className='info-box'>
        <DisplayMath
          stuff={ws.getEquationDisplay(selectedId).jsItems}
        />
        <p>{equation.name}</p>
        {Object.keys(equation.varNamesJs).map((varSymbol) =>
          <div key={varSymbol}>{equation.varNamesJs[varSymbol]} &nbsp;
            <DisplayMath
              stuff={ws.getVarDisplay(Gem.VarId(selectedId, varSymbol)).jsItems}
            /> ::&nbsp;
            <DisplayMath
              stuff={equation.dimensionsJs[varSymbol].toDisplayMath.jsItems}
            />
          </div>
        )}
        <button onClick={() => this.deleteEquation(selectedId)}>Delete</button>
      </div>
    }
    return null;
  }
  render() {
    const ws = this.state.workspace;
    this.equationRefs = {};
    this.expressionRefs = {};
    this.varRefs = {};
    this.numberRefs = {};
    const currentAction = this.state.currentAction;

    return (
      <div className="container">
        <div className="header"><h3>Buck's neato physics notebook</h3></div>

        <div className="main-app-div">
          <div className="equation-space" ref={(div) => { this.equationSpaceDiv = div; }}
            style={currentAction === DRAGGING_FROM_VAR ? {cursor: 'crosshair'} : {}}>
            <svg style={{position: 'absolute', left: 0, top: 0, height: "100%", width: "100%"}}>
              {(currentAction === DRAGGING_FROM_VAR || currentAction === DRAGGING_FROM_EXPR_VAR) &&
                this.renderVarDragLine()}
              {this.renderVarEqualityLines()}
            </svg>
            {ws.equationIds.map((equationId, idx) => {
              const pos = this.state.positions.get('equation-' + equationId);

              return <div
                key={idx}
                className="equation"
                style={{top: pos.get("y"), left: pos.get("x")}}
                ref={(div) => { this.equationRefs[equationId] = div; }}
                >
                <DisplayMath
                  onSpanMouseDown={(e) =>
                    this.handleStartDrag(e, 'equation-' + equationId, this.equationRefs[equationId],
                                        { type: 'equation', id: equationId })}
                  onVarMouseDown={(e, varId) => this.handleVariableClick(e, varId)}
                  onDoubleClick={(varId) => this.addExpression(varId)}
                  varRef={(ref, varId) => { this.varRefs[varId] = ref; }}
                  workspace={ws}
                  draggedFromVarId={this.state.draggedFromVarId}
                  idPrefix="variable-"
                  currentAction={currentAction}
                  stuff={ws.getEquationDisplay(equationId).jsItems}
                />
              </div>
            })}

            {ws.expressionIds.map((exprVarId, idx) => {
              const pos = this.state.positions.get('expression-' + exprVarId);

              return <div key={idx}
                   className="expression"
                   ref={(div) => { this.expressionRefs[exprVarId] = div; }}
                   style={{top: pos.get("y"), left: pos.get("x")}}
                   >
                <DisplayMath
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
                  stuff={ws.getExpressionDisplay(exprVarId).jsItems} />
              </div>;
            })}

            {ws.numberIds.map((numberId) => {
              const pos = this.state.positions.get('number-' + numberId);
              const number = ws.getNumber(numberId);
              return <div className='physical-number'
                          style={{position: 'absolute', top: pos.get("y"), left: pos.get("x")}}
                          ref={(div) => { this.numberRefs[numberId] = div; }}
                          key={numberId}>
                <DisplayMath
                  stuff={number.toDisplayMath.jsItems}
                  onSpanMouseDown={(e) =>
                    this.handleStartDrag(e, 'number-' + numberId, this.numberRefs[numberId], { type: 'number', id: numberId })}
                  />
              </div>;
            })}
          </div>
          <div className="sidebar">
            <div className='search-div'>
              <input className='search-bar'
                value={this.state.searchBarText}
                onChange={(e) => { this.setState({ searchBarText: e.target.value })}}
                onKeyPress={(e) => {
                  if (e.key === 'Enter') {
                    this.handleSearchBarSubmit();
                  }
                }}/>
              <button onClick={() => { this.addEquation("ke_def") }}>
              Add KE equation</button>
              <button onClick={() => { this.addEquation("pe_def") }}>
                Add PE equation</button>
            </div>
            {(() => {
              const dim = Gem.Dimension.parsePhysicalNumber(this.state.searchBarText);
              return dim ?
                <div className='physical-number'
                  onClick={() => this.handleSearchBarSubmit()}>
                  <DisplayMath stuff={dim.toDisplayMath.jsItems} /></div> :
                null;
            })()}

            {this.state.currentlySelected.type &&
              this.renderCurrentlySelectedInfo()
            }
          </div>
        </div>
      </div>
    );
  }
}

export default App;
