package workspace

import workspace.Workspace.VarId

trait WorkspaceAction {

}

case class AddEqualityAction(varId1: VarId, varId2: VarId) extends WorkspaceAction
case class RemoveEqualityAction(varId: VarId) extends WorkspaceAction
case class AddExpressionAction(varId: VarId) extends WorkspaceAction
case class DeleteExpressionAction(varId: VarId) extends WorkspaceAction
case class RewriteExpressionAction(varId: VarId, varToRemoveId: VarId, equationToUseId: Int) extends WorkspaceAction
case class AttachNumberAction(numberId: Int, varId: VarId) extends WorkspaceAction
case class DetachNumberAction(numberId: Int) extends WorkspaceAction
case class DeleteNumberAction(numberId: Int) extends WorkspaceAction


