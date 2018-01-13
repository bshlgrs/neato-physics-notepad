class NotepadsController < ApplicationController
  skip_before_action :verify_authenticity_token

  def show
    render json: {notepad: Notepad.find(params[:id])}
  end

  def index
    render json: Notepad.all
  end

  def create
    n = Notepad.create!(notepad_params.to_h)
    render json: { id: n.id }
  end

  def update
    n = Notepad.find(params[:id])
    p notepad_params
    n.update(notepad_params.to_h)
    n.content = params[:notepad][:content]
    n.save
    p n
    head :ok
  end

  private
    def notepad_params
      params.require(:notepad).permit(:title, :creator_token, :description, :id, content: {})
    end
end
