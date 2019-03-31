import React, { useState }  from 'react';
import { tags }             from '../../types/image';

import IconButton           from '../IconButton';
import panels, { PanelMap } from './panels';
import TextInput            from '../TextInput';
import styles               from './SearchSideBar.module.css';


interface Props {
    tags?: tags;
    initialPanel: string;
    children?: PanelMap;
}

const SearchSideBar = (props: Props) => {
    const [panel, changePanel] = useState<string>(props.initialPanel);
    const panelMap: PanelMap   = {
        ...panels,
        ...props.children
    };

    const Current               = panelMap[panel].panel;

    return (
        <div className={styles.Root}>
            <TextInput
                placeholder="Search"
                onKeyPress={(e) => {
                }}
            />

            <div className={styles.Buttons}>
                {
                    Object.values(panelMap).map(v =>
                        <IconButton
                            key={v.name}
                            icon={v.icon}
                            tooltip={v.tooltip}
                            active={panel === v.name}
                            onClick={() => changePanel(v.name)}
                        />
                    )
                }
            </div>

            <Current {...props} />
        </div>
    );
}

export default SearchSideBar;
