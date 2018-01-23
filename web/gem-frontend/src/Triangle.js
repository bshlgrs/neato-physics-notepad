import React from 'react';

class Triangle extends React.Component {
  render () {

    // Variables
      // Variables can be unspecified, custom, or made equal to something else.
    // Triangles should have a flippable orientation

    return <div position="relative">
      <div style={{position: "absolute", top: "70px", left: "50px", fontSize: "24px"}}>x</div>
      <div style={{position: "absolute", top: "200px", left: "70px", fontSize: "24px"}}>y</div>
      <div style={{position: "absolute", top: "90px", left: "160px", fontSize: "24px"}}>z</div>
      <div style={{position: "absolute", top: "25px", left: "130px", fontSize: "24px"}}>θ</div>
      <div style={{position: "absolute", top: "160px", left: "30px", fontSize: "24px"}}>φ</div>
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
