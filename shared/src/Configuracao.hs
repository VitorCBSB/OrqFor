-- | Configurações compartilhadas entre orquestrador e worker.
--
-- Os valores são definidos em tempo de compilação: alterar qualquer um deles
-- exige recompilar os dois lados, o que garante que ambos usem o mesmo valor.
module Configuracao
  ( portaOrquestrador
  , caminhoWebSocket
  , intervaloHeartbeatSegundos
  ) where

-- | Porta TCP em que o orquestrador atende HTTP (interface Web) e WebSocket (workers).
portaOrquestrador :: Int
portaOrquestrador = 12000

-- | Caminho HTTP em que o orquestrador aceita conexões WebSocket dos workers.
-- Conexões WebSocket em qualquer outro caminho são recusadas.
caminhoWebSocket :: String
caminhoWebSocket = "/ws"

-- | Intervalo entre heartbeats enviados pelo worker. O orquestrador deriva dele
-- o tempo de silêncio após o qual considera a conexão morta.
intervaloHeartbeatSegundos :: Int
intervaloHeartbeatSegundos = 15
