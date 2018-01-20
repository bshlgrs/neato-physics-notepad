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
        const { eqString, dimensions, varNames, constantsUsed } = content;
        return {
          name, id: parseInt(id, 10), eqString, varNames, constantsUsed, dimensionStrings: dimensions
        };
      });

      const library = Gem.EquationLibraryOps.buildFromJs(equationsForLibrary);

      const initialAction = window.localStorage.getItem("returningVisitor") ? null : 'showing-help';

      ReactDOM.render(<MainApp library={library} initialAction={initialAction}/>, document.getElementById('root'));
      window.localStorage.setItem("returningVisitor", 'true');
    });
}
registerServiceWorker();
