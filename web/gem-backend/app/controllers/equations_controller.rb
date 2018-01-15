class EquationsController < ApplicationController
  skip_before_action :verify_authenticity_token

  def create
    eq = Equation.create!(name: params[:equation][:name], content: params[:equation][:content])
    render json: { id: eq.id }
  end

  def update
    eq = Equation.find(params[:id])
    eq.name = params[:equation][:name]
    eq.content = params[:equation][:content]
    eq.save
    head :ok
  end

  def index
    render json: {equations: Equation.all.index_by(&:id)}
  end

  def destroy
    Equation.where(id: params[:id]).delete_all
    head :ok
  end
end