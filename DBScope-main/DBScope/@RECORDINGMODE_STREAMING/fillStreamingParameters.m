function [LFP, stimAmp] = fillStreamingParameters( obj, data, obj_file )
% Extract and visualize LFPs from online streaming mode
%
% Syntax:
%   [LFP, stimAmp] = FILTSTREAMINGPARAMETERS( obj, data, fname, obj_file );
%
% Input parameters:
%    * obj - object containg data
%    * data - data from json file(s)
%    * fname
%    * obj_file - object structure to contain data
%
% Output parameters:
%   LFP
%   stimAMP
%
% Example:
%   [LFP, stimAmp] = FILTSTREAMINGPARAMETERS( data, fname, obj_file ):
%
% Adapted from Yohann Thenaisie 02.09.2020 - Lausanne University Hospital
% (CHUV) https://github.com/YohannThenaisie/PerceptToolbox
%
% Available at: https://github.com/NCN-Lab/DBScope
% For referencing, please use: Andreia M. Oliveira, Eduardo Carvalho, Beatriz Barros, Carolina Soares, Manuel Ferreira-Pinto, Rui Vaz, Paulo Aguiar, DBScope: 
% a versatile computational toolbox for the visualization and analysis of sensing data from Deep Brain Stimulation, doi: 10.1101/2023.07.23.23292136.
%
% Andreia M. Oliveira, Eduardo Carvalho, Beatriz Barros & Paulo Aguiar - NCN
% INEB/i3S 2022
% pauloaguiar@i3s.up.pt
% -----------------------------------------------------------------------

% Extract LFPs
if isfield( data, 'BrainSenseTimeDomain' ) % 'BrainSenseTimeDomain'  = Streaming mode
    
    obj_file.recording_mode.n_channels  = 2;
    obj_file.recording_mode.channel_map = 1:obj_file.recording_mode.n_channels;
    obj_file.recording_mode.mode = 'BrainSenseTimeDomain';
    
    % Extract LFP data
    LFP = obj.extractLFP( data, obj_file );

    obj.streaming_parameters.time_domain.recording_mode = LFP(1).recordingMode;
    obj.streaming_parameters.time_domain.nchannels = {LFP(:).nChannels};
    obj.streaming_parameters.time_domain.channel_map = {LFP(:).channel_map};
    obj.streaming_parameters.time_domain.channel_names = {LFP(:).channel_names};
    obj.streaming_parameters.time_domain.fs = LFP(1).Fs;
    obj.streaming_parameters.time_domain.first_packet_datetimes = {LFP(:).first_packet_datetimes};
    obj.streaming_parameters.time_domain.global_packets_ID = {LFP(:).global_packets_ID};
    obj.streaming_parameters.time_domain.global_packets_size = {LFP(:).global_packets_size};
    obj.streaming_parameters.time_domain.global_packets_ticks = {LFP(:).global_packets_ticks};
    obj.streaming_parameters.time_domain.data = {LFP(:).data};
    obj.streaming_parameters.time_domain.time = {LFP(:).time};
    obj.streaming_parameters.time_domain.ecg_clean = [];

    % Extract stimulation data
    obj_file.recording_mode.mode = 'BrainSenseLfp';
    stimAmp = obj.extractStimAmp( data, obj_file );

    obj.streaming_parameters.stim_amp.recording_mode = 'BrainSenseLfp';
    obj.streaming_parameters.stim_amp.group = {stimAmp(:).group};
    obj.streaming_parameters.stim_amp.therapy_snapshot = {stimAmp(:).therapy_snapshot};
    obj.streaming_parameters.stim_amp.stim_channel_names = {stimAmp(:).stim_channel_names};
    obj.streaming_parameters.stim_amp.sensing_channel_names = {stimAmp(:).channel_names};
    obj.streaming_parameters.stim_amp.fs = stimAmp(1).Fs;
    obj.streaming_parameters.stim_amp.global_packets_ID = {stimAmp(:).global_packets_ID};
    obj.streaming_parameters.stim_amp.global_packets_ticks = {stimAmp(:).global_packets_ticks};
    obj.streaming_parameters.stim_amp.data = {stimAmp(:).data};
    obj.streaming_parameters.stim_amp.time = {stimAmp(:).time};


    obj.streaming_parameters.filtered_data.recording_mode = 'BrainSenseLfp';
    obj.streaming_parameters.filtered_data.filter_type = {};
    obj.streaming_parameters.filtered_data.bounds = {};
    obj.streaming_parameters.filtered_data.data = {};
    obj.streaming_parameters.filtered_data.original_data_ID = {};
    obj.streaming_parameters.filtered_data.data_ID = {};

    nRecordings = obj.streaming_parameters.time_domain.data;

    for c = 1:numel(nRecordings)
        if length(nRecordings{c}(1,:)) == 2
            disp('Two hemispheres available')
        elseif length(nRecordings{c}(1,:)) == 1
            disp('Only one hemispheres available')
        end
    end
end

end


