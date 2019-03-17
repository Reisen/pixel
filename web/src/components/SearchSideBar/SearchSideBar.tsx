import React, { useState } from 'react';
import styles              from './SearchSideBar.module.css';

import IconButton          from '../IconButton';
import MetaDataPanel       from './panels/MetaDataPanel';
import SettingsPanel       from './panels/SettingsPanel';
import TagsPanel           from './panels/TagPanel';
import TextInput           from '../TextInput';


interface Props {
    tags?: string[];
    initialPanel: string;
    enabledMetadata: boolean;
}

const SearchSideBar = (props: Props) => {
    const [panel, changePanel] = useState(props.initialPanel);

    return (
        <div className={styles.Root}>
            <TextInput placeholder="Search" />
            <div className={styles.Buttons}>
                <IconButton
                    icon="tag"
                    tooltip="Tag List"
                    active={panel === 'tag'}
                    onClick={() => changePanel('tags')}
                />

                <IconButton
                    icon="gears"
                    tooltip="Settings"
                    active={panel === 'settings'}
                    onClick={() => changePanel('settings')}
                />

                {
                    !props.enabledMetadata
                        ? null
                        :
                            <IconButton
                                icon="chart-radar-graph"
                                tooltip="Metadata"
                                active={panel === 'metadata'}
                                onClick={() => changePanel('metadata')}
                            />
                }
            </div>

            {
                panel === 'tags'     ? <TagsPanel {...props}/> :
                panel === 'settings' ? <SettingsPanel /> :
                panel === 'metadata' ? <MetaDataPanel /> :
                null
            }
        </div>
    );
}

export default SearchSideBar;
