import React from 'react';
import Gem from './Gem';
import BuckTex from './BuckTex';

const InfoBox = ({selectedType, selectedId, ws}) => {
  if (selectedType === "equation") {
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
        </div>
      })}
      <button className="btn btn-danger" onClick={() => this.deleteEquation(selectedId)}>
        <i className="fa fa-trash" style={{marginRight: "10px"}}/>
        Delete equation
      </button>
    </div>
  } else if (selectedType === "expression") {
    return <div className='info-box expression-info-box'>
      <BuckTex el={ws.getExpressionBuckTex(selectedId)} />
      <div>TODO: display the dimensions of the quantity, as well as its name</div>
      <button className="btn btn-danger" onClick={() => this.deleteExpression(selectedId)}>
        <i className="fa fa-trash" style={{marginRight: "10px"}}/>
        Delete expression
      </button>
    </div>
  } else if (selectedType === "number") {
    const number = ws.getNumber(selectedId);
    return <div className='info-box expression-info-box'>
      <BuckTex el={number.toBuckTex} />

      {number.toBuckTex.toString() !== number.siUnitToBuckTex.toString() && <div>
        SI units: <BuckTex inline el={number.siUnitToBuckTex} /></div>}
      <div>TODO: allow you to change the units the number is displayed in</div>
      <div>TODO: show some quantities with the same dimension with comparable sizes</div>
      <div class="btn-group" role="group">
        <button className="btn btn-danger" onClick={() => this.deleteNumber(selectedId)}>
          <i className="fa fa-trash" style={{marginRight: "10px"}}/>
          Delete number</button>
        {ws.getVarIdOfNumber(selectedId) &&
          <button className="btn btn-warning" onClick={() => this.detachNumber(selectedId)}>
            <i className="fa fa-eraser" style={{marginRight: "10px"}}/>
            Detach number
          </button>}
      </div>
    </div>
  }
  return null;
}

export default InfoBox;
