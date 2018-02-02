import React from 'react';
import BuckTex from './BuckTex';
import Gem from './Gem';

class Triangle extends React.Component {
  render () {
    const { triangle, triangleId, registerVar, onVarMouseDown } = this.props;
    // debugger;
    // Variables
      // Variables can be unspecified, custom, or made equal to something else.
    // Triangles should have a flippable orientation

    const vars = {
      "H": { top: "60px", left: "40px" },
      "A": { top: "200px", left: "70px" },
      "O": { top: "90px", left: "160px" },
      "θ": { top: "155px", left: "30px" },
      "φ": { top: "25px", left: "120px" }
    }

    // debugger;

    return <div position="relative">
      {Object.keys(vars).map((varName) => {
        const { top, left } = vars[varName];
        const varId = Gem.DiagramVarId(triangleId, varName);
        const el = this.props.workspace.diagramVarBuckTexJs(triangleId, varName);
        return <div className="diagram-variable"
          onMouseDown={(e) => onVarMouseDown(e, varId)}
          ref={(ref) => registerVar(varId, ref)}
          style={{position: "absolute", top, left}}
          key={varName}>
          {el ? <BuckTex el={el} /> : varName}
        </div>
      })}
      <svg height="210" width="160" >
        <polygon points="150,200 150,0 0,200"
          style={{strokeWidth: 1, fill: "#c1c1c1", stroke: "black", cursor: 'move'}}
          onMouseDown={this.props.onMouseDown}/>
        <path d="M 130,200 L 130,180 L 150,180"
          fill='transparent'
          stroke='black'
          strokeWidth='0.5'
          style={{cursor: 'move'}}
        />
      </svg>
    </div>
  }
}

export default Triangle;
