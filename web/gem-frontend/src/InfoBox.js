import React from 'react';
import Gem from './Gem';
import BuckTex from './BuckTex';

class InfoBox extends React.Component {
  renderEquation () {
    const {selectedId, ws} = this.props;
    const equation = ws.getEquation(selectedId);

    return <div className='info-box'>
      <BuckTex
        el={ws.getEquationBuckTex(selectedId)}
      />
      <p>{equation.name}</p>
      {equation.varsJs.map((varSymbol) => {
        const varId = Gem.VarId(selectedId, varSymbol);
        const varName = equation.varNameJs(varSymbol);
        const dimension = ws.getDimensionJs(varId);

        return <div key={varSymbol}>{varName || "unnamed"} &nbsp;
          <BuckTex inline el={ws.getVariableBuckTex(varId)} />
          {dimension && [" :: ", <BuckTex key={2} inline el={dimension.toBuckTex} />]}
          &nbsp;
          {ws.varIsEqualToAnything(varId) &&
            <a style={{fontSize: "0.8em"}} onClick={() => this.props.removeEquality(varId)}>
              (remove equality)
            </a>}
        </div>
      })}
      <button className="btn btn-danger" onClick={() => this.props.deleteEquation(selectedId)}>
        <i className="fa fa-trash" style={{marginRight: "10px"}}/>
        Delete equation
      </button>
    </div>
  }

  render () {
    const {selectedType, selectedId, ws} = this.props;
    if (selectedType === "equation") {
      return this.renderEquation();
    } else if (selectedType === "expression") {
      const dim = ws.getDimensionJs(selectedId);
      return <div className='info-box expression-info-box'>
        <BuckTex el={ws.getExpressionBuckTex(selectedId)} />
        <div>{ws.getEquation(selectedId.eqIdx).varNameJs(selectedId.varName)} ::&nbsp;
          {dim && <BuckTex inline el={dim.toBuckTex} />}</div>
        <button className="btn btn-danger" onClick={() => this.props.deleteExpression(selectedId)}>
          <i className="fa fa-trash" style={{marginRight: "10px"}}/>
          Delete expression
        </button>
      </div>
    } else if (selectedType === "number") {
      return <NumberInfoBox ws={ws}
                            numberId={selectedId}
                            deleteNumber={this.props.deleteNumber}
                            detachNumber={this.props.detachNumber}
                            changeDimension={this.props.changeDimension}
                          />
    } else if (selectedType === "triangle") {
      return <TriangleInfoBox />;
    }
    return null;
  };
}

class NumberInfoBox extends React.Component {
  constructor () {
    super();
    this.state = {
      currentAction: null,
      dimensionInputBoxContents: ""
    };
  }
  componentWillReceiveProps () {
    this.setState({
      currentAction: null,
      dimensionInputBoxContents: ""
    });
  }
  render () {
    const { ws, numberId } = this.props;
    const number = ws.getNumber(numberId);
    const dimensionInputBoxContents = this.state.dimensionInputBoxContents;
    const newDimension = Gem.Dimension.parseJs(dimensionInputBoxContents);
    const newNumberValue = newDimension ? number.value / newDimension.totalConstant : null;

    return <div className='info-box expression-info-box'>
      <BuckTex el={number.toBuckTex} />

      {number.toBuckTex.toString() !== number.siUnitToBuckTex.toString() && <div>
        SI units: <BuckTex inline el={number.siUnitToBuckTex} /></div>}

        {this.state.currentAction === null &&
          <button className='btn btn-default' onClick={() => this.setState({currentAction: 'change-units'})}>
            Change units
          </button>}
        {this.state.currentAction === 'change-units' &&
        <div>
          <input value={dimensionInputBoxContents}
                 onChange={(e) => this.setState({dimensionInputBoxContents: e.target.value})}/>
          {!newDimension && "not a dimension"}
          {newDimension && (newDimension.siDimension.equalUnits(number.siDimension) ?
            <BuckTex el={Gem.PhysicalNumber.applyWithDimension(newNumberValue, newDimension).toBuckTex} /> :
            "Units do not match"
          )}

          <div className="btn-group" role="group">
            <button className='btn btn-default'
                    onClick={() => this.props.changeDimension(numberId, newDimension)}>
              Change units to that
            </button>
            <button className='btn btn-default' onClick={() => this.setState({currentAction: null})}>
              Cancel
            </button>
          </div>
        </div>}

      <div>TODO: show some quantities with the same dimension with comparable sizes</div>
      <div className="btn-group" role="group">
        <button className="btn btn-danger" onClick={() => this.props.deleteNumber(numberId)}>
          <i className="fa fa-trash" style={{marginRight: "10px"}}/>
          Delete number</button>
        {ws.getVarIdOfNumber(numberId) &&
          <button className="btn btn-warning" onClick={() => this.props.detachNumber(numberId)}>
            <i className="fa fa-eraser" style={{marginRight: "10px"}}/>
            Detach number
          </button>}
      </div>
    </div>;
  }
}

class TriangleInfoBox extends React.Component {
  constructor () {
    super();
    this.state = {};
  }

  render () {
    return <div className="info-box">
      <p>Triangle!</p>

    </div>
  }
}

export default InfoBox;
