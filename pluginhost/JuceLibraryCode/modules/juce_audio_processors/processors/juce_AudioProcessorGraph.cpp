/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2013 - Raw Material Software Ltd.

   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3

   Details of these licenses can be found at: www.gnu.org/licenses

   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

   ------------------------------------------------------------------------------

   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.

  ==============================================================================
*/

const int AudioProcessorGraph::midiChannelIndex = 0x1000;

//==============================================================================
namespace GraphRenderingOps
{

//==============================================================================
class AudioGraphRenderingOp
{
public:
    AudioGraphRenderingOp() {}
    virtual ~AudioGraphRenderingOp()  {}

    virtual void perform (AudioSampleBuffer& sharedBufferChans,
                          const OwnedArray <MidiBuffer>& sharedMidiBuffers,
                          const int numSamples) = 0;

    JUCE_LEAK_DETECTOR (AudioGraphRenderingOp)
};

//==============================================================================
class ClearChannelOp : public AudioGraphRenderingOp
{
public:
    ClearChannelOp (const int channelNum_)
        : channelNum (channelNum_)
    {}

    void perform (AudioSampleBuffer& sharedBufferChans, const OwnedArray <MidiBuffer>&, const int numSamples)
    {
        sharedBufferChans.clear (channelNum, 0, numSamples);
    }

private:
    const int channelNum;

    JUCE_DECLARE_NON_COPYABLE (ClearChannelOp)
};

//==============================================================================
class CopyChannelOp : public AudioGraphRenderingOp
{
public:
    CopyChannelOp (const int srcChannelNum_, const int dstChannelNum_)
        : srcChannelNum (srcChannelNum_),
          dstChannelNum (dstChannelNum_)
    {}

    void perform (AudioSampleBuffer& sharedBufferChans, const OwnedArray <MidiBuffer>&, const int numSamples)
    {
        sharedBufferChans.copyFrom (dstChannelNum, 0, sharedBufferChans, srcChannelNum, 0, numSamples);
    }

private:
    const int srcChannelNum, dstChannelNum;

    JUCE_DECLARE_NON_COPYABLE (CopyChannelOp)
};

//==============================================================================
class AddChannelOp : public AudioGraphRenderingOp
{
public:
    AddChannelOp (const int srcChannelNum_, const int dstChannelNum_)
        : srcChannelNum (srcChannelNum_),
          dstChannelNum (dstChannelNum_)
    {}

    void perform (AudioSampleBuffer& sharedBufferChans, const OwnedArray <MidiBuffer>&, const int numSamples)
    {
        sharedBufferChans.addFrom (dstChannelNum, 0, sharedBufferChans, srcChannelNum, 0, numSamples);
    }

private:
    const int srcChannelNum, dstChannelNum;

    JUCE_DECLARE_NON_COPYABLE (AddChannelOp)
};

//==============================================================================
class ClearMidiBufferOp : public AudioGraphRenderingOp
{
public:
    ClearMidiBufferOp (const int bufferNum_)
        : bufferNum (bufferNum_)
    {}

    void perform (AudioSampleBuffer&, const OwnedArray <MidiBuffer>& sharedMidiBuffers, const int)
    {
        sharedMidiBuffers.getUnchecked (bufferNum)->clear();
    }

private:
    const int bufferNum;

    JUCE_DECLARE_NON_COPYABLE (ClearMidiBufferOp)
};

//==============================================================================
class CopyMidiBufferOp : public AudioGraphRenderingOp
{
public:
    CopyMidiBufferOp (const int srcBufferNum_, const int dstBufferNum_)
        : srcBufferNum (srcBufferNum_),
          dstBufferNum (dstBufferNum_)
    {}

    void perform (AudioSampleBuffer&, const OwnedArray <MidiBuffer>& sharedMidiBuffers, const int)
    {
        *sharedMidiBuffers.getUnchecked (dstBufferNum) = *sharedMidiBuffers.getUnchecked (srcBufferNum);
    }

private:
    const int srcBufferNum, dstBufferNum;

    JUCE_DECLARE_NON_COPYABLE (CopyMidiBufferOp)
};

//==============================================================================
class AddMidiBufferOp : public AudioGraphRenderingOp
{
public:
    AddMidiBufferOp (const int srcBufferNum_, const int dstBufferNum_)
        : srcBufferNum (srcBufferNum_),
          dstBufferNum (dstBufferNum_)
    {}

    void perform (AudioSampleBuffer&, const OwnedArray <MidiBuffer>& sharedMidiBuffers, const int numSamples)
    {
        sharedMidiBuffers.getUnchecked (dstBufferNum)
            ->addEvents (*sharedMidiBuffers.getUnchecked (srcBufferNum), 0, numSamples, 0);
    }

private:
    const int srcBufferNum, dstBufferNum;

    JUCE_DECLARE_NON_COPYABLE (AddMidiBufferOp)
};

//==============================================================================
class DelayChannelOp : public AudioGraphRenderingOp
{
public:
    DelayChannelOp (const int channel_, const int numSamplesDelay_)
        : channel (channel_),
          bufferSize (numSamplesDelay_ + 1),
          readIndex (0), writeIndex (numSamplesDelay_)
    {
        buffer.calloc ((size_t) bufferSize);
    }

    void perform (AudioSampleBuffer& sharedBufferChans, const OwnedArray <MidiBuffer>&, const int numSamples)
    {
        float* data = sharedBufferChans.getWritePointer (channel, 0);

        for (int i = numSamples; --i >= 0;)
        {
            buffer [writeIndex] = *data;
            *data++ = buffer [readIndex];

            if (++readIndex  >= bufferSize) readIndex = 0;
            if (++writeIndex >= bufferSize) writeIndex = 0;
        }
    }

private:
    HeapBlock<float> buffer;
    const int channel, bufferSize;
    int readIndex, writeIndex;

    JUCE_DECLARE_NON_COPYABLE (DelayChannelOp)
};


//==============================================================================
class ProcessBufferOp : public AudioGraphRenderingOp
{
public:
    ProcessBufferOp (const AudioProcessorGraph::Node::Ptr& node_,
                     const Array <int>& audioChannelsToUse_,
                     const int totalChans_,
                     const int midiBufferToUse_)
        : node (node_),
          processor (node_->getProcessor()),
          audioChannelsToUse (audioChannelsToUse_),
          totalChans (jmax (1, totalChans_)),
          midiBufferToUse (midiBufferToUse_)
    {
        channels.calloc ((size_t) totalChans);

        while (audioChannelsToUse.size() < totalChans)
            audioChannelsToUse.add (0);
    }

    void perform (AudioSampleBuffer& sharedBufferChans, const OwnedArray <MidiBuffer>& sharedMidiBuffers, const int numSamples)
    {
        for (int i = totalChans; --i >= 0;)
            channels[i] = sharedBufferChans.getWritePointer (audioChannelsToUse.getUnchecked (i), 0);

        AudioSampleBuffer buffer (channels, totalChans, numSamples);

        processor->processBlock (buffer, *sharedMidiBuffers.getUnchecked (midiBufferToUse));
    }

    const AudioProcessorGraph::Node::Ptr node;
    AudioProcessor* const processor;

private:
    Array<int> audioChannelsToUse;
    HeapBlock<float*> channels;
    int totalChans;
    int midiBufferToUse;

    JUCE_DECLARE_NON_COPYABLE (ProcessBufferOp)
};

//==============================================================================
/** Used to calculate the correct sequence of rendering ops needed, based on
    the best re-use of shared buffers at each stage.
*/
class RenderingOpSequenceCalculator
{
public:
    //==============================================================================
    RenderingOpSequenceCalculator (AudioProcessorGraph& graph_,
                                   const Array<void*>& orderedNodes_,
                                   Array<void*>& renderingOps)
        : graph (graph_),
          orderedNodes (orderedNodes_),
          totalLatency (0)
    {
        nodeIds.add ((uint32) zeroNodeID); // first buffer is read-only zeros
        channels.add (0);

        midiNodeIds.add ((uint32) zeroNodeID);

        for (int i = 0; i < orderedNodes.size(); ++i)
        {
            createRenderingOpsForNode ((AudioProcessorGraph::Node*) orderedNodes.getUnchecked(i),
                                       renderingOps, i);

            markAnyUnusedBuffersAsFree (i);
        }

        graph.setLatencySamples (totalLatency);
    }

    int getNumBuffersNeeded() const         { return nodeIds.size(); }
    int getNumMidiBuffersNeeded() const     { return midiNodeIds.size(); }

private:
    //==============================================================================
    AudioProcessorGraph& graph;
    const Array<void*>& orderedNodes;
    Array <int> channels;
    Array <uint32> nodeIds, midiNodeIds;

    enum { freeNodeID = 0xffffffff, zeroNodeID = 0xfffffffe };

    static bool isNodeBusy (uint32 nodeID) noexcept { return nodeID != freeNodeID && nodeID != zeroNodeID; }

    Array <uint32> nodeDelayIDs;
    Array <int> nodeDelays;
    int totalLatency;

    int getNodeDelay (const uint32 nodeID) const          { return nodeDelays [nodeDelayIDs.indexOf (nodeID)]; }

    void setNodeDelay (const uint32 nodeID, const int latency)
    {
        const int index = nodeDelayIDs.indexOf (nodeID);

        if (index >= 0)
        {
            nodeDelays.set (index, latency);
        }
        else
        {
            nodeDelayIDs.add (nodeID);
            nodeDelays.add (latency);
        }
    }

    int getInputLatencyForNode (const uint32 nodeID) const
    {
        int maxLatency = 0;

        for (int i = graph.getNumConnections(); --i >= 0;)
        {
            const AudioProcessorGraph::Connection* const c = graph.getConnection (i);

            if (c->destNodeId == nodeID)
                maxLatency = jmax (maxLatency, getNodeDelay (c->sourceNodeId));
        }

        return maxLatency;
    }

    //==============================================================================
    void createRenderingOpsForNode (AudioProcessorGraph::Node* const node,
                                    Array<void*>& renderingOps,
                                    const int ourRenderingIndex)
    {
        const int numIns = node->getProcessor()->getNumInputChannels();
        const int numOuts = node->getProcessor()->getNumOutputChannels();
        const int totalChans = jmax (numIns, numOuts);

        Array <int> audioChannelsToUse;
        int midiBufferToUse = -1;

        int maxLatency = getInputLatencyForNode (node->nodeId);

        for (int inputChan = 0; inputChan < numIns; ++inputChan)
        {
            // get a list of all the inputs to this node
            Array <uint32> sourceNodes;
            Array<int> sourceOutputChans;

            for (int i = graph.getNumConnections(); --i >= 0;)
            {
                const AudioProcessorGraph::Connection* const c = graph.getConnection (i);

                if (c->destNodeId == node->nodeId && c->destChannelIndex == inputChan)
                {
                    sourceNodes.add (c->sourceNodeId);
                    sourceOutputChans.add (c->sourceChannelIndex);
                }
            }

            int bufIndex = -1;

            if (sourceNodes.size() == 0)
            {
                // unconnected input channel

                if (inputChan >= numOuts)
                {
                    bufIndex = getReadOnlyEmptyBuffer();
                    jassert (bufIndex >= 0);
                }
                else
                {
                    bufIndex = getFreeBuffer (false);
                    renderingOps.add (new ClearChannelOp (bufIndex));
                }
            }
            else if (sourceNodes.size() == 1)
            {
                // channel with a straightforward single input..
                const uint32 srcNode = sourceNodes.getUnchecked(0);
                const int srcChan = sourceOutputChans.getUnchecked(0);

                bufIndex = getBufferContaining (srcNode, srcChan);

                if (bufIndex < 0)
                {
                    // if not found, this is probably a feedback loop
                    bufIndex = getReadOnlyEmptyBuffer();
                    jassert (bufIndex >= 0);
                }

                if (inputChan < numOuts
                     && isBufferNeededLater (ourRenderingIndex,
                                             inputChan,
                                             srcNode, srcChan))
                {
                    // can't mess up this channel because it's needed later by another node, so we
                    // need to use a copy of it..
                    const int newFreeBuffer = getFreeBuffer (false);

                    renderingOps.add (new CopyChannelOp (bufIndex, newFreeBuffer));

                    bufIndex = newFreeBuffer;
                }

                const int nodeDelay = getNodeDelay (srcNode);

                if (nodeDelay < maxLatency)
                    renderingOps.add (new DelayChannelOp (bufIndex, maxLatency - nodeDelay));
            }
            else
            {
                // channel with a mix of several inputs..

                // try to find a re-usable channel from our inputs..
                int reusableInputIndex = -1;

                for (int i = 0; i < sourceNodes.size(); ++i)
                {
                    const int sourceBufIndex = getBufferContaining (sourceNodes.getUnchecked(i),
                                                                    sourceOutputChans.getUnchecked(i));

                    if (sourceBufIndex >= 0
                        && ! isBufferNeededLater (ourRenderingIndex,
                                                  inputChan,
                                                  sourceNodes.getUnchecked(i),
                                                  sourceOutputChans.getUnchecked(i)))
                    {
                        // we've found one of our input chans that can be re-used..
                        reusableInputIndex = i;
                        bufIndex = sourceBufIndex;

                        const int nodeDelay = getNodeDelay (sourceNodes.getUnchecked (i));
                        if (nodeDelay < maxLatency)
                            renderingOps.add (new DelayChannelOp (sourceBufIndex, maxLatency - nodeDelay));

                        break;
                    }
                }

                if (reusableInputIndex < 0)
                {
                    // can't re-use any of our input chans, so get a new one and copy everything into it..
                    bufIndex = getFreeBuffer (false);
                    jassert (bufIndex != 0);

                    const int srcIndex = getBufferContaining (sourceNodes.getUnchecked (0),
                                                              sourceOutputChans.getUnchecked (0));
                    if (srcIndex < 0)
                    {
                        // if not found, this is probably a feedback loop
                        renderingOps.add (new ClearChannelOp (bufIndex));
                    }
                    else
                    {
                        renderingOps.add (new CopyChannelOp (srcIndex, bufIndex));
                    }

                    reusableInputIndex = 0;
                    const int nodeDelay = getNodeDelay (sourceNodes.getFirst());

                    if (nodeDelay < maxLatency)
                        renderingOps.add (new DelayChannelOp (bufIndex, maxLatency - nodeDelay));
                }

                for (int j = 0; j < sourceNodes.size(); ++j)
                {
                    if (j != reusableInputIndex)
                    {
                        int srcIndex = getBufferContaining (sourceNodes.getUnchecked(j),
                                                            sourceOutputChans.getUnchecked(j));
                        if (srcIndex >= 0)
                        {
                            const int nodeDelay = getNodeDelay (sourceNodes.getUnchecked (j));

                            if (nodeDelay < maxLatency)
                            {
                                if (! isBufferNeededLater (ourRenderingIndex, inputChan,
                                                           sourceNodes.getUnchecked(j),
                                                           sourceOutputChans.getUnchecked(j)))
                                {
                                    renderingOps.add (new DelayChannelOp (srcIndex, maxLatency - nodeDelay));
                                }
                                else // buffer is reused elsewhere, can't be delayed
                                {
                                    const int bufferToDelay = getFreeBuffer (false);
                                    renderingOps.add (new CopyChannelOp (srcIndex, bufferToDelay));
                                    renderingOps.add (new DelayChannelOp (bufferToDelay, maxLatency - nodeDelay));
                                    srcIndex = bufferToDelay;
                                }
                            }

                            renderingOps.add (new AddChannelOp (srcIndex, bufIndex));
                        }
                    }
                }
            }

            jassert (bufIndex >= 0);
            audioChannelsToUse.add (bufIndex);

            if (inputChan < numOuts)
                markBufferAsContaining (bufIndex, node->nodeId, inputChan);
        }

        for (int outputChan = numIns; outputChan < numOuts; ++outputChan)
        {
            const int bufIndex = getFreeBuffer (false);
            jassert (bufIndex != 0);
            audioChannelsToUse.add (bufIndex);

            markBufferAsContaining (bufIndex, node->nodeId, outputChan);
        }

        // Now the same thing for midi..
        Array <uint32> midiSourceNodes;

        for (int i = graph.getNumConnections(); --i >= 0;)
        {
            const AudioProcessorGraph::Connection* const c = graph.getConnection (i);

            if (c->destNodeId == node->nodeId && c->destChannelIndex == AudioProcessorGraph::midiChannelIndex)
                midiSourceNodes.add (c->sourceNodeId);
        }

        if (midiSourceNodes.size() == 0)
        {
            // No midi inputs..
            midiBufferToUse = getFreeBuffer (true); // need to pick a buffer even if the processor doesn't use midi

            if (node->getProcessor()->acceptsMidi() || node->getProcessor()->producesMidi())
                renderingOps.add (new ClearMidiBufferOp (midiBufferToUse));
        }
        else if (midiSourceNodes.size() == 1)
        {
            // One midi input..
            midiBufferToUse = getBufferContaining (midiSourceNodes.getUnchecked(0),
                                                   AudioProcessorGraph::midiChannelIndex);

            if (midiBufferToUse >= 0)
            {
                if (isBufferNeededLater (ourRenderingIndex,
                                         AudioProcessorGraph::midiChannelIndex,
                                         midiSourceNodes.getUnchecked(0),
                                         AudioProcessorGraph::midiChannelIndex))
                {
                    // can't mess up this channel because it's needed later by another node, so we
                    // need to use a copy of it..
                    const int newFreeBuffer = getFreeBuffer (true);
                    renderingOps.add (new CopyMidiBufferOp (midiBufferToUse, newFreeBuffer));
                    midiBufferToUse = newFreeBuffer;
                }
            }
            else
            {
                // probably a feedback loop, so just use an empty one..
                midiBufferToUse = getFreeBuffer (true); // need to pick a buffer even if the processor doesn't use midi
            }
        }
        else
        {
            // More than one midi input being mixed..
            int reusableInputIndex = -1;

            for (int i = 0; i < midiSourceNodes.size(); ++i)
            {
                const int sourceBufIndex = getBufferContaining (midiSourceNodes.getUnchecked(i),
                                                                AudioProcessorGraph::midiChannelIndex);

                if (sourceBufIndex >= 0
                     && ! isBufferNeededLater (ourRenderingIndex,
                                               AudioProcessorGraph::midiChannelIndex,
                                               midiSourceNodes.getUnchecked(i),
                                               AudioProcessorGraph::midiChannelIndex))
                {
                    // we've found one of our input buffers that can be re-used..
                    reusableInputIndex = i;
                    midiBufferToUse = sourceBufIndex;
                    break;
                }
            }

            if (reusableInputIndex < 0)
            {
                // can't re-use any of our input buffers, so get a new one and copy everything into it..
                midiBufferToUse = getFreeBuffer (true);
                jassert (midiBufferToUse >= 0);

                const int srcIndex = getBufferContaining (midiSourceNodes.getUnchecked(0),
                                                          AudioProcessorGraph::midiChannelIndex);
                if (srcIndex >= 0)
                    renderingOps.add (new CopyMidiBufferOp (srcIndex, midiBufferToUse));
                else
                    renderingOps.add (new ClearMidiBufferOp (midiBufferToUse));

                reusableInputIndex = 0;
            }

            for (int j = 0; j < midiSourceNodes.size(); ++j)
            {
                if (j != reusableInputIndex)
                {
                    const int srcIndex = getBufferContaining (midiSourceNodes.getUnchecked(j),
                                                              AudioProcessorGraph::midiChannelIndex);
                    if (srcIndex >= 0)
                        renderingOps.add (new AddMidiBufferOp (srcIndex, midiBufferToUse));
                }
            }
        }

        if (node->getProcessor()->producesMidi())
            markBufferAsContaining (midiBufferToUse, node->nodeId,
                                    AudioProcessorGraph::midiChannelIndex);

        setNodeDelay (node->nodeId, maxLatency + node->getProcessor()->getLatencySamples());

        if (numOuts == 0)
            totalLatency = maxLatency;

        renderingOps.add (new ProcessBufferOp (node, audioChannelsToUse,
                                               totalChans, midiBufferToUse));
    }

    //==============================================================================
    int getFreeBuffer (const bool forMidi)
    {
        if (forMidi)
        {
            for (int i = 1; i < midiNodeIds.size(); ++i)
                if (midiNodeIds.getUnchecked(i) == freeNodeID)
                    return i;

            midiNodeIds.add ((uint32) freeNodeID);
            return midiNodeIds.size() - 1;
        }
        else
        {
            for (int i = 1; i < nodeIds.size(); ++i)
                if (nodeIds.getUnchecked(i) == freeNodeID)
                    return i;

            nodeIds.add ((uint32) freeNodeID);
            channels.add (0);
            return nodeIds.size() - 1;
        }
    }

    int getReadOnlyEmptyBuffer() const noexcept
    {
        return 0;
    }

    int getBufferContaining (const uint32 nodeId, const int outputChannel) const noexcept
    {
        if (outputChannel == AudioProcessorGraph::midiChannelIndex)
        {
            for (int i = midiNodeIds.size(); --i >= 0;)
                if (midiNodeIds.getUnchecked(i) == nodeId)
                    return i;
        }
        else
        {
            for (int i = nodeIds.size(); --i >= 0;)
                if (nodeIds.getUnchecked(i) == nodeId
                     && channels.getUnchecked(i) == outputChannel)
                    return i;
        }

        return -1;
    }

    void markAnyUnusedBuffersAsFree (const int stepIndex)
    {
        for (int i = 0; i < nodeIds.size(); ++i)
        {
            if (isNodeBusy (nodeIds.getUnchecked(i))
                 && ! isBufferNeededLater (stepIndex, -1,
                                           nodeIds.getUnchecked(i),
                                           channels.getUnchecked(i)))
            {
                nodeIds.set (i, (uint32) freeNodeID);
            }
        }

        for (int i = 0; i < midiNodeIds.size(); ++i)
        {
            if (isNodeBusy (midiNodeIds.getUnchecked(i))
                 && ! isBufferNeededLater (stepIndex, -1,
                                           midiNodeIds.getUnchecked(i),
                                           AudioProcessorGraph::midiChannelIndex))
            {
                midiNodeIds.set (i, (uint32) freeNodeID);
            }
        }
    }

    bool isBufferNeededLater (int stepIndexToSearchFrom,
                              int inputChannelOfIndexToIgnore,
                              const uint32 nodeId,
                              const int outputChanIndex) const
    {
        while (stepIndexToSearchFrom < orderedNodes.size())
        {
            const AudioProcessorGraph::Node* const node = (const AudioProcessorGraph::Node*) orderedNodes.getUnchecked (stepIndexToSearchFrom);

            if (outputChanIndex == AudioProcessorGraph::midiChannelIndex)
            {
                if (inputChannelOfIndexToIgnore != AudioProcessorGraph::midiChannelIndex
                     && graph.getConnectionBetween (nodeId, AudioProcessorGraph::midiChannelIndex,
                                                    node->nodeId, AudioProcessorGraph::midiChannelIndex) != nullptr)
                    return true;
            }
            else
            {
                for (int i = 0; i < node->getProcessor()->getNumInputChannels(); ++i)
                    if (i != inputChannelOfIndexToIgnore
                         && graph.getConnectionBetween (nodeId, outputChanIndex,
                                                        node->nodeId, i) != nullptr)
                        return true;
            }

            inputChannelOfIndexToIgnore = -1;
            ++stepIndexToSearchFrom;
        }

        return false;
    }

    void markBufferAsContaining (int bufferNum, uint32 nodeId, int outputIndex)
    {
        if (outputIndex == AudioProcessorGraph::midiChannelIndex)
        {
            jassert (bufferNum > 0 && bufferNum < midiNodeIds.size());

            midiNodeIds.set (bufferNum, nodeId);
        }
        else
        {
            jassert (bufferNum >= 0 && bufferNum < nodeIds.size());

            nodeIds.set (bufferNum, nodeId);
            channels.set (bufferNum, outputIndex);
        }
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (RenderingOpSequenceCalculator)
};

//==============================================================================
// Holds a fast lookup table for checking which nodes are inputs to others.
class ConnectionLookupTable
{
public:
    explicit ConnectionLookupTable (const OwnedArray<AudioProcessorGraph::Connection>& connections)
    {
        for (int i = 0; i < connections.size(); ++i)
        {
            const AudioProcessorGraph::Connection* const c = connections.getUnchecked(i);

            int index;
            Entry* entry = findEntry (c->destNodeId, index);

            if (entry == nullptr)
            {
                entry = new Entry (c->destNodeId);
                entries.insert (index, entry);
            }

            entry->srcNodes.add (c->sourceNodeId);
        }
    }

    bool isAnInputTo (const uint32 possibleInputId,
                      const uint32 possibleDestinationId) const noexcept
    {
        return isAnInputToRecursive (possibleInputId, possibleDestinationId, entries.size());
    }

private:
    //==============================================================================
    struct Entry
    {
        explicit Entry (const uint32 destNodeId_) noexcept : destNodeId (destNodeId_) {}

        const uint32 destNodeId;
        SortedSet<uint32> srcNodes;

        JUCE_DECLARE_NON_COPYABLE (Entry)
    };

    OwnedArray<Entry> entries;

    bool isAnInputToRecursive (const uint32 possibleInputId,
                               const uint32 possibleDestinationId,
                               int recursionCheck) const noexcept
    {
        int index;

        if (const Entry* const entry = findEntry (possibleDestinationId, index))
        {
            const SortedSet<uint32>& srcNodes = entry->srcNodes;

            if (srcNodes.contains (possibleInputId))
                return true;

            if (--recursionCheck >= 0)
            {
                for (int i = 0; i < srcNodes.size(); ++i)
                    if (isAnInputToRecursive (possibleInputId, srcNodes.getUnchecked(i), recursionCheck))
                        return true;
            }
        }

        return false;
    }

    Entry* findEntry (const uint32 destNodeId, int& insertIndex) const noexcept
    {
        Entry* result = nullptr;

        int start = 0;
        int end = entries.size();

        for (;;)
        {
            if (start >= end)
            {
                break;
            }
            else if (destNodeId == entries.getUnchecked (start)->destNodeId)
            {
                result = entries.getUnchecked (start);
                break;
            }
            else
            {
                const int halfway = (start + end) / 2;

                if (halfway == start)
                {
                    if (destNodeId >= entries.getUnchecked (halfway)->destNodeId)
                        ++start;

                    break;
                }
                else if (destNodeId >= entries.getUnchecked (halfway)->destNodeId)
                    start = halfway;
                else
                    end = halfway;
            }
        }

        insertIndex = start;
        return result;
    }

    JUCE_DECLARE_NON_COPYABLE (ConnectionLookupTable)
};

//==============================================================================
struct ConnectionSorter
{
    static int compareElements (const AudioProcessorGraph::Connection* const first,
                                const AudioProcessorGraph::Connection* const second) noexcept
    {
        if (first->sourceNodeId < second->sourceNodeId)                return -1;
        if (first->sourceNodeId > second->sourceNodeId)                return 1;
        if (first->destNodeId < second->destNodeId)                    return -1;
        if (first->destNodeId > second->destNodeId)                    return 1;
        if (first->sourceChannelIndex < second->sourceChannelIndex)    return -1;
        if (first->sourceChannelIndex > second->sourceChannelIndex)    return 1;
        if (first->destChannelIndex < second->destChannelIndex)        return -1;
        if (first->destChannelIndex > second->destChannelIndex)        return 1;

        return 0;
    }
};

}

//==============================================================================
AudioProcessorGraph::Connection::Connection (const uint32 sourceNodeId_, const int sourceChannelIndex_,
                                             const uint32 destNodeId_, const int destChannelIndex_) noexcept
    : sourceNodeId (sourceNodeId_), sourceChannelIndex (sourceChannelIndex_),
      destNodeId (destNodeId_), destChannelIndex (destChannelIndex_)
{
}

//==============================================================================
AudioProcessorGraph::Node::Node (const uint32 nodeId_, AudioProcessor* const processor_) noexcept
    : nodeId (nodeId_),
      processor (processor_),
      isPrepared (false)
{
    jassert (processor != nullptr);
}

void AudioProcessorGraph::Node::prepare (const double sampleRate, const int blockSize,
                                         AudioProcessorGraph* const graph)
{
    if (! isPrepared)
    {
        isPrepared = true;
        setParentGraph (graph);

        processor->setPlayConfigDetails (processor->getNumInputChannels(),
                                         processor->getNumOutputChannels(),
                                         sampleRate, blockSize);

        processor->prepareToPlay (sampleRate, blockSize);
    }
}

void AudioProcessorGraph::Node::unprepare()
{
    if (isPrepared)
    {
        isPrepared = false;
        processor->releaseResources();
    }
}

void AudioProcessorGraph::Node::setParentGraph (AudioProcessorGraph* const graph) const
{
    if (AudioProcessorGraph::AudioGraphIOProcessor* const ioProc
            = dynamic_cast <AudioProcessorGraph::AudioGraphIOProcessor*> (processor.get()))
        ioProc->setParentGraph (graph);
}

//==============================================================================
AudioProcessorGraph::AudioProcessorGraph()
    : lastNodeId (0),
      currentAudioInputBuffer (nullptr),
      currentMidiInputBuffer (nullptr)
{
}

AudioProcessorGraph::~AudioProcessorGraph()
{
    clearRenderingSequence();
    clear();
}

const String AudioProcessorGraph::getName() const
{
    return "Audio Graph";
}

//==============================================================================
void AudioProcessorGraph::clear()
{
    nodes.clear();
    connections.clear();
    triggerAsyncUpdate();
}

AudioProcessorGraph::Node* AudioProcessorGraph::getNodeForId (const uint32 nodeId) const
{
    for (int i = nodes.size(); --i >= 0;)
        if (nodes.getUnchecked(i)->nodeId == nodeId)
            return nodes.getUnchecked(i);

    return nullptr;
}

AudioProcessorGraph::Node* AudioProcessorGraph::addNode (AudioProcessor* const newProcessor, uint32 nodeId)
{
    if (newProcessor == nullptr || newProcessor == this)
    {
        jassertfalse;
        return nullptr;
    }

    for (int i = nodes.size(); --i >= 0;)
    {
        if (nodes.getUnchecked(i)->getProcessor() == newProcessor)
        {
            jassertfalse; // Cannot add the same object to the graph twice!
            return nullptr;
        }
    }

    if (nodeId == 0)
    {
        nodeId = ++lastNodeId;
    }
    else
    {
        // you can't add a node with an id that already exists in the graph..
        jassert (getNodeForId (nodeId) == nullptr);
        removeNode (nodeId);

        if (nodeId > lastNodeId)
            lastNodeId = nodeId;
    }

    newProcessor->setPlayHead (getPlayHead());

    Node* const n = new Node (nodeId, newProcessor);
    nodes.add (n);
    triggerAsyncUpdate();

    n->setParentGraph (this);
    return n;
}

bool AudioProcessorGraph::removeNode (const uint32 nodeId)
{
    disconnectNode (nodeId);

    for (int i = nodes.size(); --i >= 0;)
    {
        if (nodes.getUnchecked(i)->nodeId == nodeId)
        {
            nodes.getUnchecked(i)->setParentGraph (nullptr);
            nodes.remove (i);
            triggerAsyncUpdate();

            return true;
        }
    }

    return false;
}

//==============================================================================
const AudioProcessorGraph::Connection* AudioProcessorGraph::getConnectionBetween (const uint32 sourceNodeId,
                                                                                  const int sourceChannelIndex,
                                                                                  const uint32 destNodeId,
                                                                                  const int destChannelIndex) const
{
    const Connection c (sourceNodeId, sourceChannelIndex, destNodeId, destChannelIndex);
    GraphRenderingOps::ConnectionSorter sorter;
    return connections [connections.indexOfSorted (sorter, &c)];
}

bool AudioProcessorGraph::isConnected (const uint32 possibleSourceNodeId,
                                       const uint32 possibleDestNodeId) const
{
    for (int i = connections.size(); --i >= 0;)
    {
        const Connection* const c = connections.getUnchecked(i);

        if (c->sourceNodeId == possibleSourceNodeId
             && c->destNodeId == possibleDestNodeId)
        {
            return true;
        }
    }

    return false;
}

bool AudioProcessorGraph::canConnect (const uint32 sourceNodeId,
                                      const int sourceChannelIndex,
                                      const uint32 destNodeId,
                                      const int destChannelIndex) const
{
    if (sourceChannelIndex < 0
         || destChannelIndex < 0
         || sourceNodeId == destNodeId
         || (destChannelIndex == midiChannelIndex) != (sourceChannelIndex == midiChannelIndex))
        return false;

    const Node* const source = getNodeForId (sourceNodeId);

    if (source == nullptr
         || (sourceChannelIndex != midiChannelIndex && sourceChannelIndex >= source->processor->getNumOutputChannels())
         || (sourceChannelIndex == midiChannelIndex && ! source->processor->producesMidi()))
        return false;

    const Node* const dest = getNodeForId (destNodeId);

    if (dest == nullptr
         || (destChannelIndex != midiChannelIndex && destChannelIndex >= dest->processor->getNumInputChannels())
         || (destChannelIndex == midiChannelIndex && ! dest->processor->acceptsMidi()))
        return false;

    return getConnectionBetween (sourceNodeId, sourceChannelIndex,
                                 destNodeId, destChannelIndex) == nullptr;
}

bool AudioProcessorGraph::addConnection (const uint32 sourceNodeId,
                                         const int sourceChannelIndex,
                                         const uint32 destNodeId,
                                         const int destChannelIndex)
{
    if (! canConnect (sourceNodeId, sourceChannelIndex, destNodeId, destChannelIndex))
        return false;

    GraphRenderingOps::ConnectionSorter sorter;
    connections.addSorted (sorter, new Connection (sourceNodeId, sourceChannelIndex,
                                                   destNodeId, destChannelIndex));
    triggerAsyncUpdate();
    return true;
}

void AudioProcessorGraph::removeConnection (const int index)
{
    connections.remove (index);
    triggerAsyncUpdate();
}

bool AudioProcessorGraph::removeConnection (const uint32 sourceNodeId, const int sourceChannelIndex,
                                            const uint32 destNodeId, const int destChannelIndex)
{
    bool doneAnything = false;

    for (int i = connections.size(); --i >= 0;)
    {
        const Connection* const c = connections.getUnchecked(i);

        if (c->sourceNodeId == sourceNodeId
             && c->destNodeId == destNodeId
             && c->sourceChannelIndex == sourceChannelIndex
             && c->destChannelIndex == destChannelIndex)
        {
            removeConnection (i);
            doneAnything = true;
        }
    }

    return doneAnything;
}

bool AudioProcessorGraph::disconnectNode (const uint32 nodeId)
{
    bool doneAnything = false;

    for (int i = connections.size(); --i >= 0;)
    {
        const Connection* const c = connections.getUnchecked(i);

        if (c->sourceNodeId == nodeId || c->destNodeId == nodeId)
        {
            removeConnection (i);
            doneAnything = true;
        }
    }

    return doneAnything;
}

bool AudioProcessorGraph::isConnectionLegal (const Connection* const c) const
{
    jassert (c != nullptr);

    const Node* const source = getNodeForId (c->sourceNodeId);
    const Node* const dest   = getNodeForId (c->destNodeId);

    return source != nullptr
        && dest != nullptr
        && (c->sourceChannelIndex != midiChannelIndex ? isPositiveAndBelow (c->sourceChannelIndex, source->processor->getNumOutputChannels())
                                                      : source->processor->producesMidi())
        && (c->destChannelIndex   != midiChannelIndex ? isPositiveAndBelow (c->destChannelIndex, dest->processor->getNumInputChannels())
                                                      : dest->processor->acceptsMidi());
}

bool AudioProcessorGraph::removeIllegalConnections()
{
    bool doneAnything = false;

    for (int i = connections.size(); --i >= 0;)
    {
        if (! isConnectionLegal (connections.getUnchecked(i)))
        {
            removeConnection (i);
            doneAnything = true;
        }
    }

    return doneAnything;
}

//==============================================================================
static void deleteRenderOpArray (Array<void*>& ops)
{
    for (int i = ops.size(); --i >= 0;)
        delete static_cast<GraphRenderingOps::AudioGraphRenderingOp*> (ops.getUnchecked(i));
}

void AudioProcessorGraph::clearRenderingSequence()
{
    Array<void*> oldOps;

    {
        const ScopedLock sl (getCallbackLock());
        renderingOps.swapWith (oldOps);
    }

    deleteRenderOpArray (oldOps);
}

bool AudioProcessorGraph::isAnInputTo (const uint32 possibleInputId,
                                       const uint32 possibleDestinationId,
                                       const int recursionCheck) const
{
    if (recursionCheck > 0)
    {
        for (int i = connections.size(); --i >= 0;)
        {
            const AudioProcessorGraph::Connection* const c = connections.getUnchecked (i);

            if (c->destNodeId == possibleDestinationId
                 && (c->sourceNodeId == possibleInputId
                      || isAnInputTo (possibleInputId, c->sourceNodeId, recursionCheck - 1)))
                return true;
        }
    }

    return false;
}

void AudioProcessorGraph::buildRenderingSequence()
{
    Array<void*> newRenderingOps;
    int numRenderingBuffersNeeded = 2;
    int numMidiBuffersNeeded = 1;

    {
        MessageManagerLock mml;

        Array<void*> orderedNodes;

        {
            const GraphRenderingOps::ConnectionLookupTable table (connections);

            for (int i = 0; i < nodes.size(); ++i)
            {
                Node* const node = nodes.getUnchecked(i);

                node->prepare (getSampleRate(), getBlockSize(), this);

                int j = 0;
                for (; j < orderedNodes.size(); ++j)
                    if (table.isAnInputTo (node->nodeId, ((Node*) orderedNodes.getUnchecked(j))->nodeId))
                      break;

                orderedNodes.insert (j, node);
            }
        }

        GraphRenderingOps::RenderingOpSequenceCalculator calculator (*this, orderedNodes, newRenderingOps);

        numRenderingBuffersNeeded = calculator.getNumBuffersNeeded();
        numMidiBuffersNeeded = calculator.getNumMidiBuffersNeeded();
    }

    {
        // swap over to the new rendering sequence..
        const ScopedLock sl (getCallbackLock());

        renderingBuffers.setSize (numRenderingBuffersNeeded, getBlockSize());
        renderingBuffers.clear();

        for (int i = midiBuffers.size(); --i >= 0;)
            midiBuffers.getUnchecked(i)->clear();

        while (midiBuffers.size() < numMidiBuffersNeeded)
            midiBuffers.add (new MidiBuffer());

        renderingOps.swapWith (newRenderingOps);
    }

    // delete the old ones..
    deleteRenderOpArray (newRenderingOps);
}

void AudioProcessorGraph::handleAsyncUpdate()
{
    buildRenderingSequence();
}

//==============================================================================
void AudioProcessorGraph::prepareToPlay (double /*sampleRate*/, int estimatedSamplesPerBlock)
{
    currentAudioInputBuffer = nullptr;
    currentAudioOutputBuffer.setSize (jmax (1, getNumOutputChannels()), estimatedSamplesPerBlock);
    currentMidiInputBuffer = nullptr;
    currentMidiOutputBuffer.clear();

    clearRenderingSequence();
    buildRenderingSequence();
}

void AudioProcessorGraph::releaseResources()
{
    for (int i = 0; i < nodes.size(); ++i)
        nodes.getUnchecked(i)->unprepare();

    renderingBuffers.setSize (1, 1);
    midiBuffers.clear();

    currentAudioInputBuffer = nullptr;
    currentAudioOutputBuffer.setSize (1, 1);
    currentMidiInputBuffer = nullptr;
    currentMidiOutputBuffer.clear();
}

void AudioProcessorGraph::reset()
{
    const ScopedLock sl (getCallbackLock());

    for (int i = 0; i < nodes.size(); ++i)
        nodes.getUnchecked(i)->getProcessor()->reset();
}

void AudioProcessorGraph::processBlock (AudioSampleBuffer& buffer, MidiBuffer& midiMessages)
{
    const int numSamples = buffer.getNumSamples();

    currentAudioInputBuffer = &buffer;
    currentAudioOutputBuffer.setSize (jmax (1, buffer.getNumChannels()), numSamples);
    currentAudioOutputBuffer.clear();
    currentMidiInputBuffer = &midiMessages;
    currentMidiOutputBuffer.clear();

    for (int i = 0; i < renderingOps.size(); ++i)
    {
        GraphRenderingOps::AudioGraphRenderingOp* const op
            = (GraphRenderingOps::AudioGraphRenderingOp*) renderingOps.getUnchecked(i);

        op->perform (renderingBuffers, midiBuffers, numSamples);
    }

    for (int i = 0; i < buffer.getNumChannels(); ++i)
        buffer.copyFrom (i, 0, currentAudioOutputBuffer, i, 0, numSamples);

    midiMessages.clear();
    midiMessages.addEvents (currentMidiOutputBuffer, 0, buffer.getNumSamples(), 0);
}

const String AudioProcessorGraph::getInputChannelName (int channelIndex) const
{
    return "Input " + String (channelIndex + 1);
}

const String AudioProcessorGraph::getOutputChannelName (int channelIndex) const
{
    return "Output " + String (channelIndex + 1);
}

bool AudioProcessorGraph::isInputChannelStereoPair (int /*index*/) const    { return true; }
bool AudioProcessorGraph::isOutputChannelStereoPair (int /*index*/) const   { return true; }
bool AudioProcessorGraph::silenceInProducesSilenceOut() const               { return false; }
double AudioProcessorGraph::getTailLengthSeconds() const                    { return 0; }
bool AudioProcessorGraph::acceptsMidi() const   { return true; }
bool AudioProcessorGraph::producesMidi() const  { return true; }
void AudioProcessorGraph::getStateInformation (juce::MemoryBlock& /*destData*/)   {}
void AudioProcessorGraph::setStateInformation (const void* /*data*/, int /*sizeInBytes*/)   {}


//==============================================================================
AudioProcessorGraph::AudioGraphIOProcessor::AudioGraphIOProcessor (const IODeviceType type_)
    : type (type_),
      graph (nullptr)
{
}

AudioProcessorGraph::AudioGraphIOProcessor::~AudioGraphIOProcessor()
{
}

const String AudioProcessorGraph::AudioGraphIOProcessor::getName() const
{
    switch (type)
    {
        case audioOutputNode:   return "Audio Output";
        case audioInputNode:    return "Audio Input";
        case midiOutputNode:    return "Midi Output";
        case midiInputNode:     return "Midi Input";
        default:                break;
    }

    return String();
}

void AudioProcessorGraph::AudioGraphIOProcessor::fillInPluginDescription (PluginDescription& d) const
{
    d.name = getName();
    d.uid = d.name.hashCode();
    d.category = "I/O devices";
    d.pluginFormatName = "Internal";
    d.manufacturerName = "Raw Material Software";
    d.version = "1.0";
    d.isInstrument = false;

    d.numInputChannels = getNumInputChannels();
    if (type == audioOutputNode && graph != nullptr)
        d.numInputChannels = graph->getNumInputChannels();

    d.numOutputChannels = getNumOutputChannels();
    if (type == audioInputNode && graph != nullptr)
        d.numOutputChannels = graph->getNumOutputChannels();
}

void AudioProcessorGraph::AudioGraphIOProcessor::prepareToPlay (double, int)
{
    jassert (graph != nullptr);
}

void AudioProcessorGraph::AudioGraphIOProcessor::releaseResources()
{
}

void AudioProcessorGraph::AudioGraphIOProcessor::processBlock (AudioSampleBuffer& buffer,
                                                               MidiBuffer& midiMessages)
{
    jassert (graph != nullptr);

    switch (type)
    {
        case audioOutputNode:
        {
            for (int i = jmin (graph->currentAudioOutputBuffer.getNumChannels(),
                               buffer.getNumChannels()); --i >= 0;)
            {
                graph->currentAudioOutputBuffer.addFrom (i, 0, buffer, i, 0, buffer.getNumSamples());
            }

            break;
        }

        case audioInputNode:
        {
            for (int i = jmin (graph->currentAudioInputBuffer->getNumChannels(),
                               buffer.getNumChannels()); --i >= 0;)
            {
                buffer.copyFrom (i, 0, *graph->currentAudioInputBuffer, i, 0, buffer.getNumSamples());
            }

            break;
        }

        case midiOutputNode:
            graph->currentMidiOutputBuffer.addEvents (midiMessages, 0, buffer.getNumSamples(), 0);
            break;

        case midiInputNode:
            midiMessages.addEvents (*graph->currentMidiInputBuffer, 0, buffer.getNumSamples(), 0);
            break;

        default:
            break;
    }
}

bool AudioProcessorGraph::AudioGraphIOProcessor::silenceInProducesSilenceOut() const
{
    return isOutput();
}

double AudioProcessorGraph::AudioGraphIOProcessor::getTailLengthSeconds() const
{
    return 0;
}

bool AudioProcessorGraph::AudioGraphIOProcessor::acceptsMidi() const
{
    return type == midiOutputNode;
}

bool AudioProcessorGraph::AudioGraphIOProcessor::producesMidi() const
{
    return type == midiInputNode;
}

const String AudioProcessorGraph::AudioGraphIOProcessor::getInputChannelName (int channelIndex) const
{
    switch (type)
    {
        case audioOutputNode:   return "Output " + String (channelIndex + 1);
        case midiOutputNode:    return "Midi Output";
        default:                break;
    }

    return String();
}

const String AudioProcessorGraph::AudioGraphIOProcessor::getOutputChannelName (int channelIndex) const
{
    switch (type)
    {
        case audioInputNode:    return "Input " + String (channelIndex + 1);
        case midiInputNode:     return "Midi Input";
        default:                break;
    }

    return String();
}

bool AudioProcessorGraph::AudioGraphIOProcessor::isInputChannelStereoPair (int /*index*/) const
{
    return type == audioInputNode || type == audioOutputNode;
}

bool AudioProcessorGraph::AudioGraphIOProcessor::isOutputChannelStereoPair (int index) const
{
    return isInputChannelStereoPair (index);
}

bool AudioProcessorGraph::AudioGraphIOProcessor::isInput() const   { return type == audioInputNode  || type == midiInputNode; }
bool AudioProcessorGraph::AudioGraphIOProcessor::isOutput() const  { return type == audioOutputNode || type == midiOutputNode; }

bool AudioProcessorGraph::AudioGraphIOProcessor::hasEditor() const                  { return false; }
AudioProcessorEditor* AudioProcessorGraph::AudioGraphIOProcessor::createEditor()    { return nullptr; }

int AudioProcessorGraph::AudioGraphIOProcessor::getNumParameters()                  { return 0; }
const String AudioProcessorGraph::AudioGraphIOProcessor::getParameterName (int)     { return String(); }

float AudioProcessorGraph::AudioGraphIOProcessor::getParameter (int)                { return 0.0f; }
const String AudioProcessorGraph::AudioGraphIOProcessor::getParameterText (int)     { return String(); }
void AudioProcessorGraph::AudioGraphIOProcessor::setParameter (int, float)          { }

int AudioProcessorGraph::AudioGraphIOProcessor::getNumPrograms()                    { return 0; }
int AudioProcessorGraph::AudioGraphIOProcessor::getCurrentProgram()                 { return 0; }
void AudioProcessorGraph::AudioGraphIOProcessor::setCurrentProgram (int)            { }

const String AudioProcessorGraph::AudioGraphIOProcessor::getProgramName (int)       { return String(); }
void AudioProcessorGraph::AudioGraphIOProcessor::changeProgramName (int, const String&) {}

void AudioProcessorGraph::AudioGraphIOProcessor::getStateInformation (juce::MemoryBlock&) {}
void AudioProcessorGraph::AudioGraphIOProcessor::setStateInformation (const void*, int) {}

void AudioProcessorGraph::AudioGraphIOProcessor::setParentGraph (AudioProcessorGraph* const newGraph)
{
    graph = newGraph;

    if (graph != nullptr)
    {
        setPlayConfigDetails (type == audioOutputNode ? graph->getNumOutputChannels() : 0,
                              type == audioInputNode ? graph->getNumInputChannels() : 0,
                              getSampleRate(),
                              getBlockSize());

        updateHostDisplay();
    }
}
