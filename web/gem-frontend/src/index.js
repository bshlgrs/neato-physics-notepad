import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import MainApp from './MainApp';
import EquationEditorApp from './EquationEditorApp';
import registerServiceWorker from './registerServiceWorker';
import Gem from './Gem';

if (window.location.pathname === "/equations") {
  ReactDOM.render(<EquationEditorApp />, document.getElementById('root'));
} else {
  fetch("/api/equations/", { headers: { 'Content-Type': 'application/json' } })
    .then((resp) => resp.json())
    .then((data) => {
      const equations = data.equations;
      const equationsForLibrary = Object.keys(equations).map((id) => {
        // const equations = data.equations;
        const { name, content} = equations[id];
        const { eqString, dimensions, varNames } = content;
        return {
          name, id: parseInt(id, 10), eqString, varNames, dimensionStrings: dimensions
        };
      });

      const library = Gem.EquationLibraryOps.buildFromJs(equationsForLibrary);
      ReactDOM.render(<MainApp library={library} />, document.getElementById('root'));
    });
}
registerServiceWorker();
