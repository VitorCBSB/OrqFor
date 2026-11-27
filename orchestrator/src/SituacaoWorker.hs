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
  , aoReceberHeartbeat
  , aoReceberResultado
  ) where

import Domain.Types (DemandaId, EstadoWorker (..))

data SituacaoWorker = SituacaoWorker
  { swEstado :: EstadoWorker
  , swConfirmada :: Bool
    -- ^ False entre o despacho de uma demanda e o 'DemandaAceita' do worker.
    -- Enquanto for False, heartbeats com estado Livre são antigos (enviados
    -- antes de o worker receber a demanda) e devem ser ignorados.
  } deriving (Show, Eq)

-- | Situação de um worker recém-conectado, com o estado informado no handshake.
situacaoInicial :: EstadoWorker -> SituacaoWorker
situacaoInicial estado = SituacaoWorker estado True

estaLivre :: SituacaoWorker -> Bool
estaLivre s = swEstado s == Livre

-- | Reserva o worker para uma demanda que acabou de ser despachada. Até o worker
-- confirmar o recebimento, a reserva não é considerada confirmada.
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

-- | Aplica o estado informado em um heartbeat. Se uma demanda foi despachada e
-- ainda não confirmada, um heartbeat Livre é anterior ao recebimento dela pelo
-- worker e é descartado, para não liberar o worker indevidamente.
aoReceberHeartbeat :: EstadoWorker -> SituacaoWorker -> SituacaoWorker
aoReceberHeartbeat estadoInformado s
  | not (swConfirmada s), estadoInformado == Livre = s
  | otherwise = SituacaoWorker estadoInformado True

-- | Chegou o resultado final de uma demanda. O worker só é liberado se estiver
-- ocupado com essa mesma demanda: um resultado reenviado após reconexão pode
-- chegar quando ele já está executando outra.
aoReceberResultado :: DemandaId -> SituacaoWorker -> SituacaoWorker
aoReceberResultado did s
  | swEstado s == Ocupada did = SituacaoWorker Livre True
  | otherwise = s
