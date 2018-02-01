import React from 'react';
import BuckTex from './BuckTex';

class Triangle extends React.Component {
  render () {
    const { triangle, triangleId, registerVar } = this.props;
    // debugger;
    // Variables
      // Variables can be unspecified, custom, or made equal to something else.
    // Triangles should have a flippable orientation

    const vars = ["H", "A", "O", "theta", "phi"];

    const buckTexForVars = vars.map(
      (varName) => this.props.workspace.diagramVarBuckTexJs(triangleId, varName)
    );

    // debugger;

    return <div position="relative">
      <div ref={(ref) => registerVar("H", ref)} style={{position: "absolute", top: "70px", left: "50px", fontSize: "24px"}}>
        {buckTexForVars[0] ? <BuckTex el={buckTexForVars[0]} /> : "H"}
      </div>
      <div ref={(ref) => registerVar("A", ref)} style={{position: "absolute", top: "200px", left: "70px", fontSize: "24px"}}>
        {buckTexForVars[1] ? <BuckTex el={buckTexForVars[1]} /> : "A"}
      </div>
      <div ref={(ref) => registerVar("O", ref)} style={{position: "absolute", top: "90px", left: "160px", fontSize: "24px"}}>
        {buckTexForVars[2] ? <BuckTex el={buckTexForVars[2]} /> : "O"}
      </div>
      <div ref={(ref) => registerVar("theta", ref)} style={{position: "absolute", top: "160px", left: "30px", fontSize: "24px"}}>
        {buckTexForVars[3] ? <BuckTex el={buckTexForVars[3]} /> : "θ"}
      </div>
      <div ref={(ref) => registerVar("phi", ref)} style={{position: "absolute", top: "25px", left: "130px", fontSize: "24px"}}>
        {buckTexForVars[4] ? <BuckTex el={buckTexForVars[4]} /> : "φ"}
      </div>
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
