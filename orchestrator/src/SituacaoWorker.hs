-- | Situação de um worker conectado, do ponto de vista do despacho, e as regras
-- de transição entre situações. As funções são puras; o estado compartilhado
-- (STM) fica em "AppState".
module SituacaoWorker
  ( SituacaoWorker (..)
  , situacaoInicial
  , estaLivre
  , aoDespachar
  , aoCancelarDespacho
  , aoConfirmarDemanda
  , aoRecusarDemanda
  , aoReceberHeartbeat
  , aoReceberResultado
  ) where

import Data.Text (Text)

import Domain.Types (DemandaId, EstadoWorker (..))

data SituacaoWorker = SituacaoWorker
  { swEstado :: EstadoWorker
  , swConfirmada :: Bool
    -- ^ False entre o despacho de uma demanda e a resposta do worker a ela
    -- ('DemandaAceita' ou 'DemandaRecusada'). Enquanto for False, heartbeats que
    -- não informam Ocupada foram enviados antes de o worker receber a demanda e
    -- devem ser ignorados.
  } deriving (Show, Eq)

-- | Situação de um worker recém-conectado, com o estado informado no handshake.
situacaoInicial :: EstadoWorker -> SituacaoWorker
situacaoInicial estado = SituacaoWorker estado True

-- | Só um worker Livre recebe demandas (Indisponivel e Ocupada não).
estaLivre :: SituacaoWorker -> Bool
estaLivre s = swEstado s == Livre

-- | Reserva o worker para uma demanda que acabou de ser despachada. Até o worker
-- responder, a reserva não é considerada confirmada.
aoDespachar :: DemandaId -> SituacaoWorker -> SituacaoWorker
aoDespachar did _ = SituacaoWorker (Ocupada did) False

-- | Desfaz a reserva quando o despacho falhou antes de a demanda ser enviada.
aoCancelarDespacho :: SituacaoWorker -> SituacaoWorker
aoCancelarDespacho _ = SituacaoWorker Livre True

-- | O worker confirmou o recebimento da demanda. Uma confirmação de outra
-- demanda é ignorada.
aoConfirmarDemanda :: DemandaId -> SituacaoWorker -> SituacaoWorker
aoConfirmarDemanda did s
  | swEstado s == Ocupada did = s { swConfirmada = True }
  | otherwise = s

-- | O worker recusou a demanda por não poder executá-la. Ele passa a constar
-- como indisponível, com o motivo, até o próximo heartbeat dizer o contrário.
aoRecusarDemanda :: DemandaId -> Text -> SituacaoWorker -> SituacaoWorker
aoRecusarDemanda did motivo s
  | swEstado s == Ocupada did = SituacaoWorker (Indisponivel motivo) True
  | otherwise = s

-- | Aplica o estado informado em um heartbeat. Se uma demanda foi despachada e o
-- worker ainda não respondeu, um heartbeat que não diz Ocupada é anterior ao
-- recebimento dela e é descartado, para não liberar o worker indevidamente nem
-- apagar a reserva.
aoReceberHeartbeat :: EstadoWorker -> SituacaoWorker -> SituacaoWorker
aoReceberHeartbeat estadoInformado s
  | not (swConfirmada s), not (ocupado estadoInformado) = s
  | otherwise = SituacaoWorker estadoInformado True
  where
    ocupado (Ocupada _) = True
    ocupado _ = False

-- | Chegou o resultado final de uma demanda. O worker só é liberado se estiver
-- ocupado com essa mesma demanda: um resultado reenviado após reconexão pode
-- chegar quando ele já está executando outra.
aoReceberResultado :: DemandaId -> SituacaoWorker -> SituacaoWorker
aoReceberResultado did s
  | swEstado s == Ocupada did = SituacaoWorker Livre True
  | otherwise = s
